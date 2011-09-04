Name:cactusjukebox
Version:1.0
Release:1
Summary:This is cactusjukebox mp3 player package.
Group: Applications/Multimedia
#指定了$RPM_BUILD_ROOT的值
BuildRoot:/var/tmp/%{name}-%{version}-%{release}-root
License:GPL
Source:cactusjukebox.tar.gz
URL:http://www.lazarus.freepascal.org
Packager:cactusjukebox
Requires:mplayer

%description
This package is a cactusjukebox RPM.

%prep
%setup -n %{name}

%build

#%prep
#%setup –c
#%install
#install -m 755 cactus_jukebox /usr/local/bin/cactus_jukebox
#rm -rf $RPM_BUILD_ROOT
#mkdir -p $RPM_BUILD_ROOT
#cp cactus_jukebox /usr/local/bin/cactus_jukebox
%install
mkdir -p $RPM_BUILD_ROOT/usr/local/bin
install -m 700 cactus_jukebox $RPM_BUILD_ROOT/usr/local/bin/

#%pre/%post/%trigger
#cp cactus_jukebox /usr/local/bin/cactus_jukebox

%clean
rm -rf $RPM_BUILD_ROOT
rm -rf %{_tmppath}/%{name}
rm -rf %{_topdir}/BUILD/%{name}

%files
#/usr/local/bin/
%defattr(-,root,root)
/usr/local/bin/cactus_jukebox
#(-,root,root)
#%{userpath}

%post
/usr/local/bin/cactus_jukebox



