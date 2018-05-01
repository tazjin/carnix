let kernel = buildPlatform.parsed.kernel.name;
    abi = buildPlatform.parsed.abi.name;
    include = includedFiles: src: builtins.filterSource (path: type:
      lib.lists.any (f:
        let p = toString (src + ("/" + f)); in
        (path == p) || (type == "directory" && lib.strings.hasPrefix path p)
      ) includedFiles
    ) src;
    updateFeatures = f: up: functions: builtins.deepSeq f (lib.lists.foldl' (features: fun: fun features) (lib.attrsets.recursiveUpdate f up) functions);
    mapFeatures = features: map (fun: fun { features = features; });
    mkFeatures = feat: lib.lists.foldl (features: featureName:
      if feat.${featureName} or false then
        [ featureName ] ++ features
      else
        features
    ) [] (builtins.attrNames feat);
in
