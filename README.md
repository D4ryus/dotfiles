Dotfiles are managed by tracking `$HOME` with git. Got the idea from [sircmpwn](https://drewdevault.com/2019/12/30/dotfiles.html).

```shell
cd ~
git init
git remote add origin <repo>
git fetch --recurse-submodules
git checkout -f master
```
