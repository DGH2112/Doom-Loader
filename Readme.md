# DOOM Loader

Author:   David Hoyle

Version:  1.0

Date:     23 May 2023

Web Page: http://https://github.com/DGH2112/Doom-Loader

## Overview

The purpose of this application was to provide a simple interface to allow me
to select one of several DOOM source port gaming engines and then be able to
select various IWADs and optionally PWAD files.

## Use

At the top of the application main window, you can configure multiple DOOM
game engines with the Add, Edit and Delete buttons. Below this is an area
where you can specify where all your WAD files are store (including in a
hierarchical folder structure). When this is configured, IWADs and PWAD will
be populated in the lower left and right panes respectivity. The application
searches recursively for WAD files so you can organise them how you would
like. Below these panes, there is an edit box for additional game options
which you would like to pass to the gaming engine selected.

There are occasion where an IWAD is actually a PWAD. HEXDD is one instance.
The applications settings are store in an INI in your user roaming folder
and you can edit this to add other exceptions if you come across them.
Once configured, the IWAD will appear in the PWAD pane instance.

The application will remember which IWADs have been associated with a PWAD
and will automatically select that IWAD when a PWAD is selected. This
selection is stored when the game engine is launched.

Likewise, the extra options are remembered for each IWAD. I might make this
configurable so it can be changed to be specific to a game engine, IWAD or
IWAD. Again, this is stored when the game engine is launched.

## Current Limitations

This app only works with DOOM and its related games like Heretic and Hexen.

## Binaries

You can download a binary of this project from the
[Releases](https://github.com/DGH2112/Doom-Loader/releases) section of
[this](https://github.com/DGH2112/Doom-Loader) Git Hub repository.

