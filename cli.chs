///
  Utility for parsing command-line commands and calling appropriate handlers.
///

export trait ParseError : Error {
  // Position of the argument that caused the error/warning.
  Int position;
}

export class ForbiddenArgName : ParseError {
  Int position;
  String argName, message;
  
  This(_position, _arg) :
    message("Non-ascii character in parameter name of $#{position} argument (`$arg`).") {}
}

export class EmptyFlagArg : ParseError {
  Int position;
  String message;
  
  This(_position) : message("No flags in $#{position} argument, it can be removed.") {}
}

export class DuplicateFlag : ParseError {
  Int position;
  String message, letter;
  
  This(_position, _letter) : message("Duplicated flag '$letter' in $#{position} argument, it can be removed.") {}
}

export class DuplicateNamedArg : ParseError {
  Int position;
  String message, name;
  
  This(_position, _name) : message("Duplicate argument '$name' at position $position.") {}
}

export class DuplicateDefaultArg : ParseError {
  Int position;
  String message, name;
  
  This(_position, _name) : message("Duplicate argument '$name' at position $position, parameter is default.") {}
}

export class NamelessAfterNamedArg : ParseError {
  Int position;
  String message;
  
  This(_position) : message("Nameless ($#position) argument cannot follow a named argument.") {}
}

export class UnknownCommand : ParseError {
  Int position;
  String message, cmd;
  
  This(_position, _cmd) : message("Unknown command '$cmd' at position $position.") {}
}

export class ConversionError : ParseError {
  Int position;
  String message, arg, value;
  type expectedType;
  
  This(_position, _arg, _value, _expectedType) : message("$#position argument '$cmd' at position $position.") {}
}

diffLength({}[]String s) => All x, y in s { x.length == y.length -> x = y };

export public class Command {
  ///
    Custom part of the help message of the command. Can be printed together with
    usage info.
  ///
  String help;
  
  ///
    Names of default parameters. Every array must be of different length. The
    number of default arguments will decide which parameters will be the default
    ones.
  ///
  {}[]String defaultParams where diffLength(defaultParams) = {};
  
  // Arguments of the command.
  []Record[String\{'flags'}, Arg[this]] args;
  
  ///
    Required parameters in conjunctive normal form. This is for specifying
    requirements dependent on the presence of more than one argument.
    To make a single argument required, you can also use its `required` property.
    
    Eg. `{ { 'a' }, { 'a', 'b'}, { 'b', 'c' } }` means ('a' is required) and ('a'
    or 'b' is required) and ('b' or 'c' is required).
    
    The default value means nothing is required.
  ///
  {}{}(String x where x keyof args) requires = { {} },
  
  // Subcommands, eg. in `chalk init .`, init is a subcommand of the command chalk.
  Record[String, Command] subcommands;
  
  where {
    // The default param is a valid param.
    defaultParams is Null
      | defaultParam keyof args
      | All x in y in defaultParam: x keyof args;
    
    // A command can have either default param(s) or subcommands, but not both.
    defaultParams is Null | subcommands.isEmpty;
  }
  
  // Allowed types of command parameters
  static let AllowedParamType = { Bool, Nat, Int, String, Float, File, Folder };
  
  // Represents a single argument of a command.
  static public class Arg<Command cmd> {
    // Help message.
    String help;
    
    // Type of the param.
    AllowedParamType type = String;
    
    ///
      Whether the argument is required. Needs to be specified if there is no
      default value.
    ///
    Bool required = default is null ? undefined : false;
    
    // Arguments that are required to be present if this argument exists.
    {}(String x where x keyof cmd.args) requires = {};
    
    ///
      Default value. If not null, the argument always exists and if not explicitly
      given, will have this value.
    ///
    ?ArgType default = null;
    
    // Shortcuts for concrete values. Char if the type of this param is Bool.
    Char|Record[Char, this.type] flags = {};
    
    // Limits possible values to those in `oneOf`.
    {}ArgType oneOf where computable((x) => x is oneOf) = this.type;
    
    // Custom validation function for this parameter.
    ?Error() validate = () => null;
    
    where {
      // A required param cannot have a default value.
      required -> default == null;
      
      // If the type of the param is bool, flags must be null or a Char.
      ArgType == Bool -> flags == null | flags is Char;
    }
  }
  
  pub static type CmdArgs = Record[keysof args, [key]() => Reflect.getKey(args, key).type];
  
  // Custom validation function that will be run before the handler.
  CmdArgs -> ?Error validate = () => null;
  
  // The function that will be called with the parsed arguments if they are valid.
  CmdArgs -> let T handler;
  
  ///
    Given arguments, validates them, returns an error if not valid, else executes
    the correct handler and returns its return value.
  ///
  execute(Folder root, []String args) -> ParseError|Promise[TODO] {
    Int i = 0;
    
    type ParsedArg = {{
      ?String name;
      ?String|Set[Char] value;
      ?Error warning;
      
      where (name == 'flags') == value is Set[Char];
    }};
    
    fn parseArg() -> ParsedArg {
      String arg = args[i];
      
      i += 1;
      
      arg.startsWith('--') && {
        for Int i = 2; i < arg.length; i += 1 {
          arg[i] == '=' && return { name: arg.slice(2, i), value: arg.slice(i + 1), warning: null };
          
          arg.isAsciiLetter(i) || return ForbiddenArgName(arg);
        };
        
        return { name: arg.slice(2), value: null, warning: null };
      };
      
      arg.startsWith('-') && {
        arg.length == 1 && return { name: 'flags', value: {}, warning: EmptyFlags() };
        
        let value = { ...arg.view(1).split() };
        
        return { name: 'flags', value, warning: value.size == arg.length - 1 ? null | DuplicateFlag() };
      };
      
      return { name: null, value: arg, warning: null };
    }
    
    Command cmd = this;
    []ParsedArg defaults = [];
    {{}} processedArgs = {};
    B named = false;
    
    for i != args.length {
      ParsedArg arg = parseArg();
      
      arg.name is Null && {
        named && return NamelessAfterNamedArg(i);
      
        subcommands != {} && {
          ?Command newCmd = Reflect.getProperty(subcommands, arg.value);
          
          newCmd ?|| return UnknownCommand(i, arg.value);
          
          cmd = newCmd;
          
          continue;
        }
        
        defaults.push(arg);
      }
      
      // This should throw error: property 'xxx' does not exist on processedArgs, but that can be ignored for a while
      Reflect.getProperty(processedArgs, arg.name) ?|| return DuplicateNamedArg(i, arg.name);
      Reflect.setProperty(processedArgs, arg.name, arg.value);
    };
    
    for ( param, value ) : Array.zipWith(cmd.defaultParams.find(arr => arr.length == defaults.length), defaults) {
      Reflect.getProperty(processedArgs, param) ?|| return DuplicateDefaultArg(i, param);
      Reflect.setProperty(processedArgs, param, value);
    }
    
    // { Bool, Nat, Int, String, Float, File, Folder }
    for ( param, schema ) : Reflect.entriesof(cmd.args) {
      let value = Reflect.getProperty(processedArgs, param);
      
      switch schema.type {
        Bool => Reflect.setProperty(processedArgs, param, switch value {
          null => true;
          "true" => true;
          "false" => false;
          String s => return-execute ConversionError(param, s, Bool);
        });
        Nat => Reflect.setProperty(processedArgs, param, switch value {
          null => return-execute ConversionError(param, 'null', Nat);
          String s => {
            ?Int i = String.parseNat(s);
            
            i ?|| return-execute ConversionError(param, s, Nat);
            
            i;
          };
        });
        Int => Reflect.setProperty(processedArgs, param, switch value {
          null => return-execute ConversionError(param, 'null', Int);
          String s => {
            ?Int i = String.parseInt(s);
            
            i ?|| return-execute ConversionError(param, s, Int);
            
            i;
          };
        });
        String => value;
        Float => Reflect.setProperty(processedArgs, param, switch value {
          null => return-execute ConversionError(param, 'null', Float);
          String s => {
            ?Float i = String.parseFloat(s);
            
            i ?|| return-execute ConversionError(param, s, Float);
            
            i;
          };
        });
        File {}
        Folder {}
      }
    }
    
    await async-for ( param, schema ) : Reflect.entriesof(cmd.args) {
      let path = Reflect.getProperty(processedArgs, param);
      
      switch schema.type {
        File => {
          File|Error f = await root.openFile(path);
          
          f is Error && return-execute f;
          
          Reflect.setProperty(processedArgs, param, f);
        }
        Folder => {
          Folder|Error f = await root.openFolderile(path);
          
          f is Error && return-execute f;
          
          Reflect.setProperty(processedArgs, param, f);
        }
      }
    }
    
    return cmd.handler(processedArgs);
  }
}};
