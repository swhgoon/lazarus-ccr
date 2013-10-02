Command Line Configuration


== JSON Storage ==

The configuration can be stored in JSON format.
Root values are

 executable  : string - the name of executable (don't have to match the actual binary name)
              must be the same accross multiple versions
 version     : string - the executable version.                                                
 testkey     : string - the test command-line key that needs to be passed to the executable
              to retrieve the test value
 testValue   : string - the value that's unique to this version of executable
 option      : array  - an array of options, where each option is described as json object
 fromversion : string - the version of the same executable that configuration file can be used.
                        thus "incremental" configuration files are allowed.
                        however, previous version of the file must be available as well.
                        Any new values or options 

Option values are:

  key        : string (required) - the command-line text that needs to be added to a parameter
                        if the option is selected or any value is assigned to this option
                        (in case any value is accepted, such as: int, size, filepath, dirpath, etc)     
  name       : string (optional) - code name of the option (the same name should be shared accross multiple 
                        versions of the executable - to store the value); 
  section    : string (optional) - the section of command line option. 
  subsection : string (optional) - the sub section of the section  ... todo: make a better description here
  masterkey  : string (optional) - the "key" prefix that should be used to combining multiple values into 
                        a single key entry. Example:
                        two switches -Ct -Co will be combined into -Cto, if both have -C as master key.           
  display    : string (optional) - the display name of the option. (if blank, name is used) 
  condition  : string (optional) - free form condition string  (see FPC Condition below)
  type       : string (default switch) - the type of an option. Available types are:
                            switch   - simple enabled or disabled
                            select   - multple options are available (expected "options" to be defined)
                            filename - name of a file
                            dirname  - name of a directory
                            int      - an integer value
                            string   - a free form string 
                            -        - (dash) if type is a dash, the option is removed from this version
                                       of the compiler. (used with "fromversion" configuration)
  options    : array  (optional) - a list of available values. 
  mutiple    : boolean (default false) - multiple entries of the same value is allowed.   typically 
                         is used for filename or dirnames. However, even for dirname of filename
                         it still defaults to false and must be reinitialized;                  
  alias      : string (optional) - the current key is considered to be deprecated is, the better
                         key to be used is specified by the "alias" field
               

Example of JSON Storage:

{
  "executable":"fpc",
  "version":"2.6.0",
  "testkey":"-iV",
  "testValue":"2.6.0",
  options: [
    {
      section:"execopt",
      key:"Xm",
      name:"generatelinkmap",
      display:"Generate Link Map"
    },
    { 
      section:"codegen",
      key: "Cp%value%",
      display: "Instruction set",
      name: "instructionset",
      options: [
        { value: "80386", condition:"i386,x86_64" }
        ,{ value: "PENTIUM", condition:"i386,x86_64"  }
        ,{ value: "PENTIUM2", condition:"i386,x86_64"  }
        ,{ value: "PENTIUM3", condition:"i386,x86_64"  }
        ,{ value: "PENTIUM4", condition:"i386,x86_64"  }
        ,{ value: "PENTIUMM", condition:"i386,x86_64"  }
      ]
    }
  ]
}

== FPC Condition ==

Free Pascal Compiler condition define for what target (CPU, CPU-OS) the option or value of an option 
is available. The format follows the FPC convention of target platforms naming:
  CPU-OS
 or
  CPU

Example, if an option is available for Intel 386 (32-bit machines), the condition should be set to
  i386
If an option is specific to Windows on i386 machine, the condition would be
  i386-win32
(No whitespaces are allowed between)

If an option is available for multple platforms, each condition has to be comma separated:
  i386,x86_64,arm-linux
  


