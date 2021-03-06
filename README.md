EmbDeploy
=========

http://vldgeorgiev.wordpress.com/2014/04/29/automated-deployment-of-a-delphi-osx-project-from-the-command-line/

(c) Vladimir Georgiev, 2013

Automated deployer for Embarcadero RAD Studio projects.
Uses the paclient.exe tool from Embarcadero to automate deploying projects to remote hosts from the command line. By default it is possible to deploy a project to OSX or another host only from the Delphi IDE, and not from a command line or script. This makes it harder to write automated build scripts.
The "embdeploy" tool simplifies this by parsing the project and issuing commands to paclient.exe to deploy the project files.

Embdeploy also has to option to execute custom commands on the remote host, e.g. "copy", "rm", "chmod", "codesign", etc. The command has to be enclosed in double quotes and can contain additional single quotes inside. The inside quotes might have to be escaped, depending on how you call Embdeploy.
The command is executed from the folder above the remote project root. E.g. if the project is for OSX and called "myproject" it will be deployed to "<paclient profile folder>/myproject.app" and the command executed from the <paclient profile folder>.
There is a parameter $PROOT that can be used inside commands and is replaced with the name of the project folder, which is "myproject.app" in the case above.

There is also an option to produce a ZIP archive of the deployment files. It is useful for creating a ZIP of an OSX project ".app" bundle without deploying it to OSX first. This makes it easier to use an automated build script and upload the files from Windows only without switching to OSX. It can only be used on non-sandboxed applications, because the codesigning is done on OSX only.

Vladimir Georgiev, 2013
