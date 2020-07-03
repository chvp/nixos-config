self: super: {
  zeroadPackages = super.zeroadPackages.override { newScope = extra: self.newScope ({ stdenv = self.stdenvAdapters.impureUseNativeOptimizations self.stdenv; } // extra); };
}
