# ----------------------------------------------------------------
    use strict;
    use Test::More tests => 26;
    BEGIN { use_ok('XML::FeedPP') };
# ----------------------------------------------------------------
    my $ftitle = "Title of the site";
    my $fdesc  = "Description of the site";
    my $link1 = "http://www.perl.org/";
# ----------------------------------------------------------------
	my $xmlns_media = 'http://search.yahoo.com/mrss';
	my $media_content_url 		= "http://www.kawa.net/xp/images/xp-title-512.gif";
	my $media_content_type		= "image/gif";
	my $media_content_width		= 512;
	my $media_content_height 	= 96;
	my $media_title_value		= "media title";
	my $media_text_value		= "media value";
	my $media_text_type			= "html";
	my $media_thumbnail_url		= "http://www.kawa.net/xp/images/xp-title-256.gif";
	my $media_thumbnail_width	= 256;
	my $media_thumbnail_height	= 48;
	my $media_credit_value 		= "credit value";
	my $media_credit_scheme 	= "urn:kawanet:tags";
# ----------------------------------------------------------------
	my $media_hash = {
		'media:content@url'		=>	$media_content_url,
		'media:content@type'	=>	$media_content_type,
		'media:content@width'	=>	$media_content_width,
		'media:content@height'	=>	$media_content_height,
		'media:title'			=>	$media_title_value,
		'media:text'			=>	$media_text_value,
		'media:text@type'		=>	$media_text_type,
		'media:thumbnail@url'	=>	$media_thumbnail_url,
		'media:thumbnail@width'	=>	$media_thumbnail_width,
		'media:thumbnail@height' =>	$media_thumbnail_height,
		'media:credit@scheme'	=>	$media_credit_scheme,
		'media:credit'			=>	$media_credit_value,
	};
# ----------------------------------------------------------------
    my $feed1 = XML::FeedPP::RSS->new();
    $feed1->title( $ftitle );
# ----------------------------------------------------------------
	$feed1->xmlns( 'xmlns:media' => $xmlns_media );
	is( $xmlns_media, $feed1->xmlns('xmlns:media'), 'xmlns:media' );
# ----------------------------------------------------------------
	my $item1 = $feed1->add_item( $link1 );
	$item1->set( %$media_hash );
	foreach my $key ( sort keys %$media_hash ) {
		is( $item1->get($key), $media_hash->{$key}, $key );
	}
# ----------------------------------------------------------------
	my $source1 = $feed1->to_string();
	my $feed2 = XML::FeedPP::RDF->new();
	$feed2->merge( $source1 );
	my $item2 = $feed2->get_item(0);
	foreach my $key ( sort keys %$media_hash ) {
		is( $item2->get($key), $media_hash->{$key}, $key );
	}
# ----------------------------------------------------------------
;1;
# ----------------------------------------------------------------
