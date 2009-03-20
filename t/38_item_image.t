# ----------------------------------------------------------------
    use strict;
    use Test::More tests => 4;
    BEGIN { use_ok('XML::FeedPP') };
# ----------------------------------------------------------------
    my $feed = XML::FeedPP::RDF->new();
    $feed->title("Example");
    $feed->link("http://example.org/feed");
    my $item = $feed->add_item("http://example.org/item");
    $item->title("Example 1");
    $item->attached_image(undef, { url => "http://example.org/image.gif", type => 'image/gif' });
    test_item($item);
    my $feed2 = XML::FeedPP::RDF->new($feed->to_string);
    test_item($feed2->get_item(0));
    my $feed3 = XML::FeedPP::RSS->new();
    $feed3->title("Example 3");
    $feed3->link("http://example.org/feed3");
    my $item3 = $feed->add_item("http://example.org/item-X");
    $item3->enclosure({ url => "http://example.org/image.gif", type => 'image/gif' });
    test_item($item3);
# ----------------------------------------------------------------
sub test_item {
    my ($item) = @_;
    my $attach = $item->attached_image();
    is($attach->{url}, "http://example.org/image.gif");
#    is($attach->{type}, "image/gif");
}
