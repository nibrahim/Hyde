(setq hyde/git/remote "upstream" ; The name of the remote to which we should push
      hyde/git/remote "master"   ; The name of the branch on which your blog resides
      hyde/deploy-command  "rsync -vr _site/* nkv@ssh.hcoop.net:/afs/hcoop.net/user/n/nk/nkv/public_html/nibrahim.net.in/" ; Command to deploy
      )

