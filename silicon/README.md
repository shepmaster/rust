# Common setup

1. Download and install Xcode or the Xcode Command Line Tools version
   12.2 or newer. If you have multiple versions installed, set it as
   your default:

    ```
    xcode-select -s /Applications/Xcode.app/Contents/Developer/
    ```

1. Install [rustup][] 1.23 or newer.

1. Use rustup to install Rust. Note that no native aarch64 stable
   build of Rust has been produced yet, so you will need to use Rust
   1.48 **beta** (2020-11-24 bd26e4e544992e52f208 or newer) or the
   **nightly** channel until Rust 1.49 is released.

[rustup]: https://rustup.rs

# Cross-compiling from x86_64 to aarch64

1. Add the appropriate target

    ```
    rustup target add aarch64-apple-darwin
    ```

1. Compile your code

    ```
    SDKROOT=$(xcrun -sdk macosx11.0 --show-sdk-path) \
    MACOSX_DEPLOYMENT_TARGET=$(xcrun -sdk macosx11.0 --show-sdk-platform-version) \
    cargo build --target=aarch64-apple-darwin
    ```

# Compiling on native aarch64 hardware

1. Compile your code

    ```
    cargo build
    ```

# Porting notes

If you get any compilation errors...

1. The current macOS `cc` executables have a problem with codesigning
   when run under Rosetta2 on aarch64 hardware. This can occur if
   _anything_ in the process chain is x86_64 (including but not
   limited to: your terminal emulator, the shell, rustup, cargo,
   rustc).

   You can usually work around this through some clever use of
   environment variables or intermediate shell scripts. For example,
   if your `rustup` is still a x86_64 binary (see `file $(which
   rustup)` to check) you can directly invoke `cargo` / `rustc`,
   avoiding rustup:

   ```
   RUSTC=$(rustup which rustc) $(rustup which cargo) build
   ```

   See also:

     - [Building aarch64 rustup using x86_64 rustup](https://github.com/rust-lang/rustup/issues/2413#issuecomment-729216870)
     - [Why does my native arm64 application built using an x86_64 build system fail to be code signed unless I remove the previous executable?](https://stackoverflow.com/q/64830671/155423)
     - [Why does my native application compiled on Apple Silicon sometimes build as arm64 and sometimes build as x86_64?](https://stackoverflow.com/q/64830635/155423)

1. Ensure you have updated to the latest version of any
   dependencies. Some crates have known changes to compile for
   aarch64-apple-darwin:

    - cc (use 1.0.60 or above)
    - libc (use 0.2.73 or above)
    - libz
    - openssl-src (use 111.12.0+1.1.1h or above)
    - *ring* (use 0.16.17 or above)
    - winit (use 0.24.0 or above)

1. Do you get an error like

    ```
    arch: posix_spawn: sh: Bad CPU type in executable
    ```

    This means you don't have Rosetta2 installed yet, so you can't run
    x86_64 binaries. Open an x86_64 program from the Finder or try via
    the command line with: `softwareupdate --install-rosetta`.
