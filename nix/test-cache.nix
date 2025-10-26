{pkgs}: rec {
  # Creates a cached test results derivation
  # testCommand: string - the command to run the test (e.g., "${testBinary}/bin/test")
  # testName: string - descriptive name for the test (e.g., "libhnefatafl-test")
  mkTestResults = testCommand: testName:
    pkgs.runCommand "${testName}-results" {}
    ''
      echo "Running ${testName}..." >&2
      mkdir -p $out

      # Run tests and capture both output and exit code
      set +e  # Don't exit on test failure
      ${testCommand} > $out/test-results.txt 2>&1
      TEST_EXIT_CODE=$?
      set -e  # Re-enable exit on error

      # Store the exit code for later use
      echo "$TEST_EXIT_CODE" > $out/exit-code

      # Add metadata to the results
      echo "" >> $out/test-results.txt
      echo "---" >> $out/test-results.txt
      echo "Test run completed at $(date)" >> $out/test-results.txt
      echo "Test command: ${testCommand}" >> $out/test-results.txt
      echo "Exit code: $TEST_EXIT_CODE" >> $out/test-results.txt
    '';

  # Creates a cached test runner app
  # testResults: derivation - the output of mkTestResults
  # appName: string - name for the app script (e.g., "test-libhnefatafl")
  mkTestApp = testResults: appName: {
    type = "app";
    program =
      (
        pkgs.writeShellScript "${appName}" ''
          set -eEuo pipefail

          # Check if test-results are cached
          if nix path-info ${testResults} >/dev/null 2>&1; then
            IS_CACHED=true
          else
            IS_CACHED=false
          fi

          # This will only run tests if source/dependencies changed
          # If cached, it returns instantly with the cached result
          TEST_RESULTS="${testResults}"

          # Read the test exit code
          EXIT_CODE=$(cat "$TEST_RESULTS/exit-code")

          if [ "$EXIT_CODE" -eq 0 ] && [ "$IS_CACHED" = true ]; then
            echo "cached success"
          elif [ "$EXIT_CODE" -ne 0 ]; then
            cat "$TEST_RESULTS/test-results.txt"
          fi

          echo "Test results stored at: $TEST_RESULTS" >&2

          exit "$EXIT_CODE"
        ''
      )
      .outPath;
  };

  # Convenience function that creates both test results and app
  # testCommand: string - the command to run the test
  # testName: string - descriptive name for the test
  # system: string - the system platform
  mkCachedTest = testCommand: testName: let
    testResults = mkTestResults testCommand;
  in {
    inherit testResults;
    app = mkTestApp testResults "test-${testName}";
  };
}
