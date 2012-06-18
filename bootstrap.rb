
def colortheme_from_git(user, repo, theme)
  `cd ~/src`
  `git clone git://github.com/#{user}/#{repo}.git` unless File.directory?(File.expand_path("~/src/#{repo}"))
  `cd #{repo}`
  `ln -s color-theme-#{theme}.el ~/lib/emacs/themes/`
end

def colortheme_install()
  colortheme = 'color-theme-6.6.0'

  unless File.directory?(File.expand_path('~/lib/color-theme'))
    `cd ~/Downloads`
    `wget http://download.savannah.gnu.org/releases/color-theme/#{colortheme}.tar.gz`
    `tar -zxf #{colortheme}.tar.gz`
    `mv #{colortheme} ~/lib/`
    `ln -s ~/lib/#{colortheme} ~/lib/color-theme`
  end

  `mkdir -p ~/lib/emacs/themes`
  colortheme_from_git("crafterm", "twilight-emacs", "twilight")
  colortheme_from_git("bbatsov", "zenburn-emacs", "zenburn")
  colortheme_from_git("olegshaldybin", "color-theme-railscasts", "railscasts")
  colortheme_from_git("sellout", "emacs-color-theme-solarize", "color-theme-solarize")

end

colortheme_install()

