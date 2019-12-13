# aftersave.el

When developing software, I tend to notice sometimes that I do repetitive tasks
on my session; at some point, I stop and create a quick function on my
`*scratch*` that executes a bunch of steps in one go and add an
`after-save-hook` for it on my current buffer.

This package allows me to not even think about hooks, and just register it
through an interactive command.

![aftersave](https://raw.github.com/roman/aftersave.el/assets/aftersave_el.gif)

## Usage

1. Put on your `.emacs.d/init.el`
  ```elisp
  (require 'aftersave)
  ```
2. Then `M-x aftersave/add-hook`
3. Select your preferred function to execute after a save
4. Profit!

If you at some point, you want to stop execute the given function after a save:

1. `M-x aftersave/remove-hook`
2. Pick the function you want to remove
3. Done!

## Local hooks

If you use `Ctrl-u` before excuting the `aftersave` commands, you will add the
`after-save-hook` to the current buffer only.

## Credits

Code inspired by ideas from [Tavis Rudd](https://twitter.com/tavisrudd)
