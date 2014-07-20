Adding an About property to your component
==================================

Copy the files aboutcomponentunit.pas and license.lrs to your component directory.

1 ) Rename aboutcomponentunit.pas to about<yourcomponentname>unit.pas
2 ) In the pas file rename the Unit to match (1)
3 ) Open the renamed pas file and do Search/Replace to change all instances of "TAboutComponent" to TAbout<yourcomponentname>
4 ) In the line TAboutComponent = Class(TComponent), change the ancestor to your component's ancestor (if it's not TComponent)
5 ) Add the edited pas file to your component's package
6 ) In your component's class declaration, change it's ancestor to TAbout<yourcomponentname> (from step 3)
7 ) Compile, install and see the new clickable 'About' property in your component!

Configuring the About property dialog
=============================
In your component's Constructor Create() set some, all or none of the following properties:

  AboutBoxComponentName (string)
  AboutBoxWidth (integer)
  AboutBoxHeight (integer)
  AboutBoxDescription (string - can contain LineEndings)
  AboutBoxBackgroundColor (TColor, like clWhite)
  AboutBoxFontName (string)
  AboutBoxFontSize (integer)
  AboutBoxVersion (string)
  AboutBoxAuthorname (string)
  AboutBoxOrganisation (string)
  AboutBoxAuthorEmail (string)
  AboutBoxLicenseType (string e.g. 'GPL', ModifiedGPL' etc)

You will have to recompile and reinstall your component to see the results.

=========
June 2014