#!/bin/bash
ghcid --target=rumor --run=":! ghcid --target=doctests --run=\":! ghcid --target=test-rumor --run\""
