#!/usr/bin/env bash
nix-shell -p haskellPackages.wai-app-static --command "warp -d result/bin/snowman.jsexe -p 8080"
