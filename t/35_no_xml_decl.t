# ----------------------------------------------------------------
    use strict;
    use Test::More tests => 25;
    BEGIN { use_ok('XML::FeedPP') };
# ----------------------------------------------------------------
{
    my $rss = <<'EOT';
<rss version="2.0">
    <channel>
        <title>kawa.net</title>
        <link>http://www.kawa.net/</link>
    </channel>
</rss>
EOT

    my $rdf = <<'EOT';
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://purl.org/rss/1.0/" xmlns:dc="http://purl.org/dc/elements/1.1/">
    <channel rdf:about="http://www.kawa.net/">
        <title>kawa.net</title>
        <link>http://www.kawa.net/</link>
    </channel>
</rdf:RDF>
EOT

    my $atom03 = <<'EOT';
<feed xmlns="http://www.w3.org/2005/Atom">
    <title>kawa.net</title>
    <link rel="alternate" href="http://www.kawa.net/" />
</feed>
EOT

    my $atom10 = <<'EOT';
<feed xmlns="http://purl.org/atom/ns#" version="0.3">
    <title type="text/plain">kawa.net</title>
    <link rel="alternate" type="text/html" href="http://www.kawa.net/" />
</feed>
EOT

    my $xml = '<?xml version="1.0" encoding="UTF-8" ?>';

    # with xml decl
    &test_main( 'XMLDecl RSS 2.0',  $xml.$rss );
    &test_main( 'XMLDecl RSS 1.0',  $xml.$rdf );
    &test_main( 'XMLDecl Atom 0.3', $xml.$atom03 );
    &test_main( 'XMLDecl Atom 1.0', $xml.$atom10 );

    # without xml decl
    &test_main( 'No XMLDecl RSS 2.0',  $rss );
    &test_main( 'No XMLDecl RSS 1.0',  $rdf );
    &test_main( 'No XMLDecl Atom 0.3', $atom03 );
    &test_main( 'No XMLDecl Atom 1.0', $atom10 );
}
# ----------------------------------------------------------------
sub test_main {
    my $title  = shift;
    my $source = shift;

    my $feed = XML::FeedPP->new($source);
    ok( $feed, 'load: '.$title );
    is( +$feed->title, 'kawa.net', 'title: '.$title );
    is( +$feed->link, 'http://www.kawa.net/', 'link: '.$title );
}
# ----------------------------------------------------------------
