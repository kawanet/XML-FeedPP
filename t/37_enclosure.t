# ----------------------------------------------------------------
    use strict;
    use Test::More tests => 9;
    BEGIN { use_ok('XML::FeedPP') };
# ----------------------------------------------------------------
    my $feed = XML::FeedPP::RSS->new();
    $feed->title("Example");
    $feed->link("http://example.org/feed");
    my $item = $feed->add_item("http://example.org/item");
    $item->title("Example 1");
    $item->enclosure({ url => "http://example.org/image.png", type => 'image/png', length => 3200 });
    test_feed('XML::FeedPP::RSS', 0);
    test_feed('XML::FeedPP::RDF', 1);
    test_feed('XML::FeedPP::Atom::Atom10', 1);
# ----------------------------------------------------------------
sub test_feed {
    my ($class, $multiple) = @_;
    my $feed2 = $class->new();
    $feed2->merge($feed);
    my $item2 = $feed2->get_item(0);
    is_deeply($item2->enclosure,
              { url => "http://example.org/image.png", type => 'image/png', length => 3200 },
              "merged enclosure");
    if($multiple) {
        my $complex = [{ url => "http://example.org/image.png", type => 'image/png', length => 3200 },
                       { url => "http://example.org/image.gif", type => 'image/gif', length => 4200 }];
        $item2->enclosure($complex);
        is_deeply([$item2->enclosure], $complex, "several enclosures");
    }
    is($item2->link, "http://example.org/item", "link preserves after enclosure manipulations");
}
