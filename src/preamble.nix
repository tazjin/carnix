let kernel = buildPlatform.parsed.kernel.name;
    abi = buildPlatform.parsed.abi.name;
    include = includedFiles: src:
      # The comments assume the `include` field of the Cargo.toml like:
      # include = [ "foo", "bar" ]
      # and the package is being built from /mypackage
      #
      # includeFiles == [ "foo" "bar" ]
      let
        # and includedFileAbsolutePaths == [ "/mypackage/foo" "/mypackage/bar" ]
        includedFileAbsolutePaths = builtins.map (relativePath: toString (src + ("/" + relativePath))) includedFiles;

        # Return true only when a possible path exactly matches an
        # absolute path, ie:
        #
        #     checkExactMatch "/mypackage/foo" == true
        #     checkExactMatch "/mypackage/foo/bar" == false
        #     checkExactMatch "/mypackage/baz" == false
        checkExactMatch = possiblePath: builtins.elem possiblePath includedFileAbsolutePaths;

        # Return true only when a possible path is a suffix to a
        # directory described by the include.
        #
        #     checkPrefixMatch "/mypackage/buzfoo" == false
        #     checkPrefixMatch "/mypackage/foo" == false
        #     checkPrefixMatch "/mypackage/foo/bar" == true
        #     checkPrefixMatch "/mypackage/baz/foo" == false
        checkPrefixMatch = possiblePath: lib.lists.any
          (acceptablePrefix: lib.strings.hasPrefix "${acceptablePrefix}/" possiblePath)
          includedFileAbsolutePaths;

      in builtins.filterSource (possiblePath: _type:
          builtins.trace "${possiblePath}" ((checkExactMatch possiblePath) || (checkPrefixMatch possiblePath))
      )
      src;
    updateFeatures = f: up: functions: builtins.deepSeq f (lib.lists.foldl' (features: fun: fun features) (lib.attrsets.recursiveUpdate f up) functions);
    mapFeatures = features: map (fun: fun { features = features; });
    mkFeatures = feat: lib.lists.foldl (features: featureName:
      if feat.${featureName} or false then
        [ featureName ] ++ features
      else
        features
    ) [] (builtins.attrNames feat);
in
