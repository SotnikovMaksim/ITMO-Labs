#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my @lines = <>;
my $formatted_text = "";
my $skip_empty_lines = 0;
my $processing_content = 0;

foreach (@lines) {
    # Remove HTML tags and their attributes.
    s/<[^>]*>//g;

    # Start processing when a non-blank line is encountered.
    if (!($processing_content || /^\s+$/)) {
        $processing_content = 1;
    }

    # Skip initial blank lines.
    next if $processing_content == 0;

    # Handle consecutive blank lines in the content.
    if (/^\s+$/) {
        $skip_empty_lines = 1;
        next;
    }

    # Add a single newline for consecutive blank lines.
    if ($skip_empty_lines) {
        $skip_empty_lines = 0;
        $formatted_text .= "\n";
    }

    # Trim leading and trailing whitespace and replace multiple spaces with a single space.
    s/^\s+|\s+$//g;
    s/\s+/ /g;

    # Append the processed line to the result.
    $formatted_text .= $_ . "\n";
}

print($formatted_text);
