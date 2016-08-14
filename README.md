# Emacs Setup

This repository is my ([Dimitri Fontaine](http://tapoueh.org)) Emacs Setup.
I had my
[Dot Emacs Banckruptcy](https://www.emacswiki.org/emacs/DotEmacsBankruptcy)
a very long time ago now, well before we had package managers and such.

Actually, I had to write [el-get](https://github.com/dimitri/el-get) in
order to solve the problem I had with maintaining those external packages
that the setup depend on. At this time
[Tom Tromey's ELPA](http://tromey.com/elpa/) existed already, but that was
it.

As a result my Emacs Setup is still a mess that nobody should get
inspiration from. See about my
[Emacs Kicker](https://github.com/dimitri/emacs-kicker) project for
inspiration, or some other well known initiatives like
[Technomancy's Emacs Starter Kit](https://github.com/technomancy/emacs-starter-kit)
or even [SpaceMacs](http://spacemacs.org) if you're so inclined.

# Usage

This setup is my own personnal setup and I have no intention to fix it so
it's easier for others to use. I will review and maybe even accept *Pull
Requests* when they make sense for me. You'd better have your own Emacs
Setup tailored entirely for your own needs anyway.

# Portability

This Emacs Setup has support for MacOSX, Linux and to some extend, Windows.
I played around with producing an USB key full of relocatable GNU binaries
so that I could easily use Emacs on any workstation. This part of the
portability of the setup is still quite a toy idea tho.

# Dependencies

Mainly handled via El-Get, see `dim-packages.el`. Some personal
initialisation of the packages live in `packages.d`. Some libs have not been
upgraded to proper el-get packages I could then depend on, because I am lazy
or have better things to do with my time, see the `lib` directory where I
used to _vendor-in_ anything I would use.

# Windows, Frames, Workflow

This whole setup is based on the central usage of
[EScreen](https://www.emacswiki.org/emacs/EmacsScreen), which allow to
maintain several *Window Configurations* in parallel and easily switch from
one to another.

Adding to that, the package `buffer-move.el` allows for some easy live
editing of the *window configuration*, and the package `windmove.el` allows
for easy navigation. Of course this setup also uses
[Emacs Switch Window](https://github.com/dimitri/switch-window) that I wrote
when being fed-up with Emacs default `C-x o` keybinding.

The setup also provides quite advanced features to detect the screen size
and adapt current Emacs frame size to that (I use ```C-M-` runs the command
dim:adapt-frames-to-screen-dimensions``` for that). Then the IRC setup uses
[rcirc](https://www.emacswiki.org/emacs/rcirc) and has a whole window setup
function depending on the current frame size too.

Even the [Gnus](https://www.emacswiki.org/emacs/Gnus) setup is heavily
specialized in terms of the frame size.
