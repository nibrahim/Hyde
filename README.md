Introduction
------------

Hyde.el is an Emacs major mode to help create blogs with the excellent
[Jekyll](http://jekyllrb.com/) blogging system. It comes with a front
end (`hyde.el`), a version control backend (`hyde-git.el`) and an
slightly modified version of the stock markdown editing mode that
gives you a few bells and whistles while writing posts (`hyde-md.el`).


Installation
------------

### Hyde.el
Download the all the `hyde-*.el` files and put them somewhere. Once
you do that, add the directory where you put it to your load path like
so and `require` it.


    (add-to-list 'load-path "/path/to/hyde*.el")
    (require 'hyde)

### Jekyll

There are no hyde specific things you'll have to do for your Jekyll
installation. However, there is a `.hyde.el` file which you can drop
into your blog directory which contains blog specific settings. This
file will is not something that jekyll is aware of so you might want
to update your `_config.yml` to not process this file.


Operation
---------
This mode is a simply a wrapper for a number of shell commands that
are used to create and deploy the site. It doesn't maintain any local
state (in the form of status files etc.) so if you change your
repository manually outside it, just refreshing the buffer will bring
it upto date.

It's tailored to the way I work. I keep my posts in a `git` repository
(although I do have a crude DVCS abstraction layer if you're using
`hg` or any other such system). I make changes, commit them and push
the repository to `github` (you can, for example, see this files
source at
[github](https://github.com/nibrahim/nibrahim.net.in/blob/master/_posts/2010-11-11-hyde_%3A_an_emacs_mode_for_jekyll_blogs.markdown)).
After that, I make the site using `jekyll` manually and then copy it
over to my webspace using `rsync`. I don't use any of the git hooks
(yet).

Customisation
-------------
    
Following are the variables affecting your blog and its management that you can customise

  * `hyde-home` : The default root directory of your blog
  * `hyde-deploy-dir` : The directory where `jekyll` will generate the site for you to deploy
  * `hyde-posts-dir` : The directory that will contain the actual posts
	  (this is relative to `hyde-home` and is `_posts` by default).
  * `hyde-drafts-dir` : The directory that will contain the post
	  drafts (this is relative to `hyde-home` and is `_drafts` by
	  default. You might want to _ignore_ this directory in
	  `_config.yml` ).
  * `hyde/deploy-command` : The command used to deploy the site. `scp`,
	  `rsync` or whatever else you might please.
  * `hyde/jekyll-command` : The command used to run jekyll to generate the blog. You can add bits to take care of rvm for you here. 		  
  * `hyde/hyde-list-posts-command` : The command used to list the posts in the hyde buffer. It is set to `"/bin/ls -1tr "` by default which will produce a chronologically ordered list. You can change it if you prefer alphabetic or something else. 
  * `hyde/git/remote` : The remote to which the push command should send changes to. `origin` by default.
  * `hyde/git/remote-branch` : The branch on which you blog resides and to which the push will happen. `master` by default.

When you start hyde using `M-x hyde`, it will prompt you for the directory where your blog resides. All the paths mentioned above (`posts-dir` etc.) are all relative to this. The directory that you specify here will override `hyde-home` mentioned above. 

Customising any of the above will set them globally (i.e. for all blogs managed by jekyll on your system). If you have multiple blogs which you're managing using jekyll, you can drop a `.hyde.el` file into the blog directory where you can manually set any of these variables. In this file, you can manually change any of these variables using `setq`. There is a `sample-dot-hyde.el` file which shows you how. These settings override the global ones mentioned above and you can have different such settings for different blogs on your system. 

The following are commands and predicates used to handle the version control backend. You can change them if you want to add support for a version control system other than `git`. If it works for you, please send me a pull request and I'll integrate it. 

  * `hyde/vc-uncommittedp` : Predicate to check whether the file is uncommitted
  * `hyde/vc-unpushedp` : Predicate to check whether the file is not yet pushed
  * `hyde/vc-pushedp` :  Predicate to check if the file has been pushed (inverse of the above)
  * `hyde/vc-add` : Command to add the file
  * `hyde/vc-commit` : Command to commit a file
  * `hyde/vc-push` : Command to push the local changes to the remote end.
  * `hyde/vc-rename` : Command used to rename a file.
  
Interface
---------
  
The main interface looks like the following screenshot

![Hyde screenshot](https://github.com/nibrahim/Hyde/raw/master/Screenshot.png)

The list of posts are presented on top along with a key of what the
letters before the post names mean. The post names are also colourised
accordingly

The keys you can use at this time are

* `n` : Create a new draft.
* `c` : Commit the current post
* `P` : Push all pending commits (this is only a VC push. Not
  deployment).
* `j` : Run jekyll and create the new version of the site
* `d` : Deploy the site.
* `g` : Refresh posts (useful if you've done something by hand
  earlier)
* `p` : Promote a post from a draft to a a published post
* `q` : Quit hyde.  
* `RET` : Open the current post for editing.

The markdown mode in which the buffers open up for editing is slightly
modified. It has a few extra covenience bindings

* `C-c C-c` : Save file and finish editing
* `C-c C-v` : Preview file (this is a markdown preview so extra
  `liquid` tags will not work). 

Octopress
---------
Hyde supports Octopress by default. The only thing you have to do is to create a directory under `source` for draft posts and
edit the `.hyde.el` file with the right commands. To see an example, check out the [emacsmovies.org source](http://github.com/nibrahim/emacsmovies.org) repository.


License
-------
This program is licensed under the GNU General Public License Version
3. Please check the LICENSE file for the full text of the license.

To Do
-----

* Proper previews using a local Jekyll server
* Keep state of deployment so that we know what posts have been
  deployed and what not.
* Use `comint` instead of shell commands to do all the work. 
