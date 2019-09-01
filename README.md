# karakul
Shake-based MCU build &amp; upload

This package is a tool for building MCU projects (mostly focused on AVR & STM32). It uses the [Shake](https://shakebuild.com/) library. 

Karakul relies on the [GNU Embedded Toolchain for Arm](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-rm) to generate binaries from your source code. To program you board various open-source or vendor provided programmers are used depending on the specific MCU.

To build karakul you need a [stack](https://docs.haskellstack.org/en/stable/README/) installation. Once you have stack installed you can build and install karakul by:

```sh
cd karakul
stack install
```

You may need to adjust your path to include the relevant bin directory. Further, to make the build-tool more friently for frequent accces you can add shell aliases or a wrapper script to your taste. My own script is called `mk` and contains:

```sh
:

if [ -f build.mk ]
then
    exec shakemcu $*
else
    for P in `find . -name build.mk -print`
    do (
        DIR=`dirname $P`
        echo $DIR
        cd $DIR
        mk
    ) done
fi
```
