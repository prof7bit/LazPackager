lazdebian
=========

What is it?
-----------
Lazdebian is a Lazarus plugin (a package that you can install in your IDE)
that will enable you to create an installable (optionally signed) binary
Debian package from any existing Lazarus project.

You can also use it to create a so called "source package" that is a
tarball with the umodified original sources along with signed hashes and a
signed diff that adds all the necessary Debian voodoo to allow automated
building and packaging on their build farm.


How does it work?
-----------------
When you invoke lazdebian it will make a copy of the source tree, create
the necessary control file, rules and other meta data with information from
your project settings on the fly using (configurable) templates and then
invoke the debuild tool to create either a binary or a source package,
optionally sign it with debsign (using your gpg key) and optionally upload
it with dput to your Launchpad PPA where it will be enqueued for building
automatically.

Lazbuild will store some additional information about your project in the
project file (.lpi) in the CustomData section. Some needed information can
be inferred from the Lazarus project settings automatically, other things
need to be configured separately. It will also store any changes and
additional tweaks you made to the default debian templates (Makefile,
control, rules, changelog, copyright) in the project file.

For using the signing and uploading feature you will need to have gpg
installed and have a valid key-pair for your email address. When uploading
to Launchpad this must be the **same** email address and gpg key you used
when signing their "code of conduct" and also the same email address that
you configure in lazdebian.

Lazdebian will not store any confidential information in the config file,
the only password you need to provide is the passphrase for your gpg key
and for this it will pop up a console and let gpg ask it from you
directly. You do not need your Launchpad password for uploading because
they will authenticate you and your uploaded files by your valid gpg
signature.


Does it work yet?
-----------------
Almost, but no upload yet, detection of version number is currently broken, 
default templates still have minor errors, not yet ready for production.