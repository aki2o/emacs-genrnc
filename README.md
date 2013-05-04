What's this?
============

This is a extension of Emacs that generate RELAX NG Compact Schema from RELAX NG Schema, XML Schema and DTD for nXML-mode.

Feature
=======

### Available Schema Format

* RELAX NG Schema. File extention is "rng".
* XML Schema. File extension is "xsd".
* DTD. File extension is "dtd".

### Regist include/import schema automatically

If regist the schema that has include/import, you give only the schema to the regist command.

### Update/Delete/Rename schema from TypeID

Registed schema has *TypeID* on nXML-mode.  
You can update/delete/rename schema by using *TypeID*.

Install/Configuration
=====================

### Required

* Java Runtime Environment

### Install

2013/05/03 Now during an application for registration in el-get.

I recommend using el-get for installing this extension.  
Downloading manually or using git command are OK,
but installing each the following dependency is required in this case.

### For Manually

Download this archive and expand it. Then, add the directory that has genrnc.el to load-path.

### For Using git command

    git clone https://github.com/aki2o/emacs-genrnc

Then, add the directory that has genrnc.el to load-path.

### Dependency

* [deferred.el](https://github.com/kiwanami/emacs-deferred)
* [log4e.el](https://github.com/aki2o/log4e)
* [yaxception.el](https://github.com/aki2o/yaxception)

### Configure

    (require 'genrnc)
    (setq genrnc-user-schemas-directory "~/.emacs.d/schema")

About other customization, execute `customize-group "genrnc"`.

Usage
=====

This extension provides the following commands.

* genrnc-regist-url ... Regist schema from URL.
* genrnc-regist-file ... Regist schema from Filepath.
* genrnc-update-user-schema ... Update schema from TypeID.
* genrnc-delete-user-schema ... Delete schema from TypeID.
* genrnc-rename-user-schema ... Rename schema from TypeID.
* genrnc-clear-cache ... Do cleaning the cache about schema statement and user input.

This extension don't provide Keymap.  
Then, for using above commands, do M-x genrnc-regist-url or bind key by yourself.

Information
===========

### Regist/Update

This extension display the following message when finished some action.

    [GENRNC] Finished ...

If the schema has many include/import, The action maybe take a little time.  
If the action is failed, the following message is displayed.

    [GENRNC] Failed ...

The following message is displayed occasionally.

    deferred error : ...

In the case, see \*Messages\* buffer for checking the finished message is outputed.  
If the finished message is outputed, the action is finished normally.

### Use schema

Do M-x rng-set-document-type-and-validate on nXML-mode.  
By default, the command is bound "C-c C-s C-t".  
Check the schema is listed, and select it.

### Do Cleaning cache

If you retry the action when it failed, the message about relation of cache maybe is displayed.  
In the case, do M-x genrnc-clear-cache.  
It's no problem that you execute the command frequently.

### Unknown Schema Location

In XML Schema, import element maybe don't have schemaLocation attribute.  
In the case, if don't know the schema location, show the following prompt.

    [GENRNC] Including not located ns:'...'. Input PATH or URL locating this: 

Then, input right URL/Filepath of the schema.

Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
* deferred.el ... 0.3.1
* log4e.el ... 0.1
* yaxception.el ... 0.1


**Enjoy!!!**

