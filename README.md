Dotfiles are managed by tracking `$HOME` with git. Got the idea from [sircmpwn](https://drewdevault.com/2019/12/30/dotfiles.html).

```shell
cd $HOME
git init --bare .dotfiles
alias dotfiles="git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
dotfiles config --local status.showUntrackedFiles no
dotfiles remote add origin <repo>
dotfiles fetch --recurse-submodules
dotfiles checkout -f master
```
