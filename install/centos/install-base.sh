#!/bin/sh

curl http://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm --output /tmp/epel-release-latest-7.noarch.rpm

rpm -ivh /tmp/epel-release-latest-7.noarch.rpm

yum upgrade -y
yum install stow -y
yum install zsh -y
yum install wget -y


# git 2.x
yum remove git -y
yum install https://centos7.iuscommunity.org/ius-release.rpm -y
yum install git2u yum-utils -y
yum-config-manager --disable ius

