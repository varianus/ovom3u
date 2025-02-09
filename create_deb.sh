#!/bin/bash
# set -x
if [ "$BASE" = "" ]; then
  BASE=$(pwd)
fi 

ARCH=$(./get_architecture.sh )

BIN_DIR=$BASE/bin/
DEBSRCDIR=$BASE/packages/debian/
PACKAGES_DIR=$BASE/packages
SRCDIR=$BASE/src

MKDIR="install -m 755 -d "
INSTALLFILE="install -c -m 644"
INSTALLEXE="install -c -m 755"

PROG_VER=$(sed -e "s/[^0-9.a-zA-Z]//g" $SRCDIR/version.inc)


##
copylanguage()
{
$MKDIR $DEBSRCDIR/usr/share/locale/$1
$MKDIR $DEBSRCDIR/usr/share/locale/$1/LC_MESSAGES
$INSTALLFILE  $SRCDIR/language/ovom3u.$1.po $DEBSRCDIR/usr/share/locale/$1/LC_MESSAGES/ovom3u.po
}


rm -Rf $DEBSRCDIR
$MKDIR $DEBSRCDIR
$MKDIR $DEBSRCDIR/usr
$MKDIR $DEBSRCDIR/usr/bin
$MKDIR $DEBSRCDIR/usr/share
$MKDIR $DEBSRCDIR/usr/share/doc
$MKDIR $DEBSRCDIR/usr/share/doc/ovom3u
$MKDIR $DEBSRCDIR/usr/share/ovom3u
$MKDIR $DEBSRCDIR/usr/share/applications
$MKDIR $DEBSRCDIR/usr/share/pixmaps
$MKDIR $DEBSRCDIR/usr/share/man
$MKDIR $DEBSRCDIR/usr/share/man/man1
$MKDIR $DEBSRCDIR/usr/share/locale
$MKDIR $DEBSRCDIR/usr/share/lintian
$MKDIR $DEBSRCDIR/usr/share/lintian/overrides

$MKDIR $DEBSRCDIR/DEBIAN

$INSTALLEXE  -s $BIN_DIR/ovom3u $DEBSRCDIR/usr/bin
$INSTALLFILE  $SRCDIR/images/ovom3u.png $DEBSRCDIR/usr/share/pixmaps/ovom3u.png
$INSTALLFILE  $PACKAGES_DIR/ovom3u.desktop $DEBSRCDIR/usr/share/applications

##  Add language files
copylanguage it
##

$INSTALLFILE  $PACKAGES_DIR/copyright $DEBSRCDIR/usr/share/doc/ovom3u
$INSTALLFILE  $PACKAGES_DIR/lintian-overrides $DEBSRCDIR/usr/share/lintian/overrides/ovom3u

#gzip -c --best $BASE/DOC/manpages/ovom3u.1 > $DEBSRCDIR/usr/share/man/man1/ovom3u.1.gz
#chmod 0644 $DEBSRCDIR/usr/share/man/man1/ovom3u.1.gz

gzip -c --best $PACKAGES_DIR/changelog > $DEBSRCDIR/usr/share/doc/ovom3u/changelog.gz
chmod 0644 $DEBSRCDIR/usr/share/doc/ovom3u/changelog.gz

INSTALLEDSIZE=$(du -0 -xs --apparent-size --block-size=1024 $DEBSRCDIR/usr | cut -f 1)
sed -e 's/:INSTALLEDSIZE/'$INSTALLEDSIZE'/;s/:VERSION/'$PROG_VER'/;s/:ARCHITECTURE/'$ARCH'/' $BASE/packages/control > $DEBSRCDIR/DEBIAN/control
chmod 0644 $DEBSRCDIR/DEBIAN/control

cd $DEBSRCDIR
find usr -type f | xargs md5sum >> $DEBSRCDIR/DEBIAN/md5sums
chmod 0644 $DEBSRCDIR/DEBIAN/md5sums

fakeroot dpkg --build $DEBSRCDIR $PACKAGES_DIR/ovom3u-$PROG_VER-gtk2-$ARCH.deb



