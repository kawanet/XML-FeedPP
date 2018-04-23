use strict;    # file scoped

package XML::FeedPP;

use Carp qw(croak);
use Time::Local;
use XML::TreePP;
use Scalar::Util qw(reftype);

our $VERSION = '0.96';

our $RSS20_VERSION  = '2.0';
our $ATOM03_VERSION = '0.3';

our $XMLNS_RDF    = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
our $XMLNS_RSS    = 'http://purl.org/rss/1.0/';
our $XMLNS_DC     = 'http://purl.org/dc/elements/1.1/';
our $XMLNS_ATOM03 = 'http://purl.org/atom/ns#';
our $XMLNS_ATOM10 = 'http://www.w3.org/2005/Atom';
our %XMLNS_NOCOPY = map +($_ => 1), qw(xmlns xmlns:rdf xmlns:dc xmlns:atom);

our %TREEPP_OPTIONS = (
    force_array => [qw( item rdf:li entry )],
    first_out   => [qw( -xmlns:rdf -xmlns -rel -type url title link )],
    last_out    => [qw( description image item items entry -width -height )],
    user_agent  => "XML-FeedPP/$VERSION ",
);

our $MIME_TYPES = { reverse qw(
    image/bmp                       bmp
    image/gif                       gif
    image/jpeg                      jpeg
    image/jpeg                      jpg
    image/png                       png
    image/svg+xml                   svg
    image/x-icon                    ico
    image/x-xbitmap                 xbm
    image/x-xpixmap                 xpm
)};

our %IS_FEED_METHOD = map +($_ => 1), qw(
    title
    description
    language
    copyright
    link
    pubDate
    image
    set
);

our %IS_ITEM_METHOD = map +($_ => 1), qw(
    title
    description
    category
    author
    link
    guid
    pubDate
    image
    set
);

our @IMPLEMENTED_FEEDS = qw(
   XML::FeedPP::RSS
   XML::FeedPP::RDF
   XML::FeedPP::Atom::Atom10
   XML::FeedPP::Atom::Atom03
   XML::FeedPP::Atom
);

sub new {
    my $package = shift;
    my( $init, $source, @rest ) = &XML::FeedPP::Util::param_even_odd(@_);
    my $do_autodetect = $package eq __PACKAGE__;

    croak "No feed source"
        if $do_autodetect && !$source;

    my $self = bless {}, $package;
    $self->load($source, @rest)
        if $source;

    if($do_autodetect) {
        my $class = $self->detect_format
            or croak "Unsupported feed format: ". join( " ", sort keys %$self);
        bless $self, $class;
    }

    $self->validate_feed if $source;
    $self->init_feed;
    $self->elements(@$init) if ref $init;
    $self;
}

sub validate_feed {
    my $self = shift;
    return if $self->test_feed(@_);

    my $class = ref $self;
    croak "Invalid $class feed format: ".join( " ", sort keys %$self);
}

sub detect_format {
    my $self = shift;
    foreach my $plugin (@IMPLEMENTED_FEEDS) {
        my $hit = $plugin->test_feed($self);
        return $plugin if $hit;
    }
    undef;
}

sub load {
    my ($self, $source, %args) = @_;
    defined $source or croak "No feed source";

    my $method = $args{-type};
    if ( ! $method ) {
        if ( $source =~ m#^https?://#s ) {
            $method = 'url';
        }
        elsif ( $source =~ m#(?:\s*\xEF\xBB\xBF)?\s*
                             (<(\?xml|!DOCTYPE|rdf:RDF|rss|feed)\W)#xis ) {
            $method = 'string';
        }
        elsif ( $source !~ /[\r\n]/ && -f $source ) {
            $method = 'file';
        }
        else {
            croak "Invalid feed source: $source";
        }
    }

    my @opts = map +($_ => $args{$_}), grep !/^-/, keys %args;
    my $tpp  = XML::TreePP->new(%TREEPP_OPTIONS, @opts);

    my $tree
      = $method eq 'url'    ? $tpp->parsehttp(GET => $source)
      : $method eq 'string' ? $tpp->parse($source)
      : $method eq 'file'   ? $tpp->parsefile($source)
      : croak "Invalid load type: $method";

    ref $tree or croak "Loading failed: $source";
    %$self = %$tree;    # absorb myself
    $self;
}

sub to_string {
    my $self = shift;
    my( $args, $encode, @rest ) = XML::FeedPP::Util::param_even_odd(@_);
    $args ||= \@rest;
    my @opts = ( output_encoding => $encode ) if $encode;
    my $tpp = XML::TreePP->new( %TREEPP_OPTIONS, @opts, @$args );
    $tpp->write( $self, $encode );
}

sub to_file {
    my $self = shift;
    my $file = shift;
    my( $args, $encode, @rest ) = XML::FeedPP::Util::param_even_odd(@_);
    $args ||= \@rest;
    my @opts = ( output_encoding => $encode ) if $encode;
    my $tpp = XML::TreePP->new( %TREEPP_OPTIONS, @opts, @$args );
    $tpp->writefile( $file, $self, $encode );
}

sub merge {
    my ($self, $source) = (shift, shift);
    my $target = ref $source ? $source : XML::FeedPP->new($source, @_);

    $self->merge_channel($target);
    $self->merge_item($target);
    $self->normalize;
    $self;
}

sub merge_channel {
    my $self   = shift;
    my $target = shift or return;

    ref $self eq ref $target
      ? $self->merge_native_channel($target)
      : $self->merge_common_channel($target);
}

sub merge_item {
    my $self   = shift;
    my $target = shift or return;
    $self->add_item($_) for $target->get_item;
}

sub merge_common_channel {
    my $self   = shift;
    my $target = shift or return;

    unless($self->title) {
        my $title = $target->title;
        $self->title($title) if defined $title;
    }

    unless($self->description) {
        my $descr = $target->description;
        $self->description($descr) if defined $descr;
    }

    unless($self->link) {
        my $link = $target->link;
        $self->link($link) if defined $link;
    }

    unless($self->language) {
        my $lang = $target->language();
        $self->language($lang) if defined $lang;
    }

    unless($self->copyright) {
        my $right = $target->copyright;
        $self->copyright($right) if defined $right;
    }

    unless($self->pubDate) {
        my $pubDate = $target->pubDate;
        $self->pubDate($pubDate) if defined $pubDate;
    }

    my @images = $self->image;
    unless($images[0]) {
        @images = $target->image;
        $self->image(@images) if $images[0];
    }

    my %have_ns = map +($_ => 1), $self->xmlns;
    foreach my $ns ($target->xmlns) {
        $self->xmlns($ns, $target->xmlns($ns))
            unless $XML::FeedPP::XMLNS_NOCOPY{$ns} || $have_ns{$ns}++;
    }

    $self->merge_module_nodes($self->docroot, $target->docroot);
    $self;
}

sub add_clone_item {
    my $self    = shift;
    my $srcitem = shift or return;

    my $link    = $srcitem->link or return;
    my $dstitem = $self->add_item($link);

    if ( ref $dstitem eq ref $srcitem ) {
        XML::FeedPP::Util::merge_hash( $dstitem, $srcitem );

        # A new item starts with a link, and then a merge.  Setting the
        # link often affects the guid.  The merge will not set the guid
        # anymore.  So, let's overrule that.
        if(my $guid = $srcitem->{guid}) {
            $dstitem->{guid} = $guid;
        }

        return $dstitem;
    }

    my $title = $srcitem->title;
    $dstitem->title($title) if defined $title;

    my $description = $srcitem->description;
    $dstitem->description($description) if defined $description;

    my $category = $srcitem->category;
    $dstitem->category($category) if defined $category;

    my $author = $srcitem->author;
    $dstitem->author($author) if defined $author;

    my $guid = $srcitem->guid;
    $dstitem->guid($guid) if defined $guid;

    my $pubDate = $srcitem->pubDate;
    $dstitem->pubDate($pubDate) if defined $pubDate;

    $self->merge_module_nodes( $dstitem, $srcitem );
    $dstitem;
}

sub merge_module_nodes {
    my ($self, $item1, $item2) = @_;

    foreach my $key ( grep /\:/, keys %$item2 ) {
        next if $key =~ /^-?(dc|rdf|xmlns)\:/;
        $item1->{$key} = $item2->{$key}; #XXX deep copy would be better
    }
}

sub normalize {
    my $self = shift;
    $self->normalize_pubDate;
    $self->sort_item;
    $self->uniq_item;
}

sub normalize_pubDate {
    my $self = shift;

    foreach my $item ($self->get_item) {
        my $date = $item->get_pubDate_native;
        $item->pubDate($date) if $date;
    }

    my $date = $self->get_pubDate_native;
    $self->pubDate($date) if $date;
}

sub xmlns {
    my ($self, $ns, $url) = @_;
    my $root = $self->docroot;

    if ( !defined $ns ) {
        my @list = grep /^-xmlns(:\S|$)/, keys %$root;
        return map { (/^-(.*)$/)[0] } @list;
    }

    $root->{"-$ns"} = $url if defined $url;
    $root->{"-$ns"};
}

sub get_pubDate_w3cdtf {
    my $self = shift;
    my $date = $self->get_pubDate_native;
    XML::FeedPP::Util::get_w3cdtf($date);
}

sub get_pubDate_rfc1123 {
    my $self = shift;
    my $date = $self->get_pubDate_native;
    XML::FeedPP::Util::get_rfc1123($date);
}

sub get_pubDate_epoch {
    my $self = shift;
    my $date = $self->get_pubDate_native;
    XML::FeedPP::Util::get_epoch($date);
}

sub call {
    my ($self, $name) = (shift, shift);

    my $class = __PACKAGE__."::Plugin::".$name;
    unless($class->VERSION) {
        my $pmfile = $class;
        $pmfile =~ s#::#/#g;
        $pmfile .= ".pm";
        local $@;
        eval { require $pmfile };
        croak "$class failed: $@" if $@;
    }

    return $class->run($self, @_);
}

sub elements {
    my ($self, @args) = @_;
    while(@args) {
        my ($key, $val) = (shift @args, shift @args);
        if($IS_FEED_METHOD{$key}) {
            $self->$key($val);
        }
        else {
            $self->set($key => $val);
        }
    }
}

sub match_item {
    my $self = shift;

    my @out;
    foreach my $item ($self->get_item) {
        my @args = @_;
        my $unmatch = 0;
        while(@args) {
            my ($key, $test) = (shift @args, shift @args);
            my $got  = $IS_ITEM_METHOD{$key} ? $item->$key : $item->get($key);
            unless ( $got =~ $test ) {
                $unmatch++;
                last;
            }
        }
        unless ( $unmatch ) {
            return $item unless wantarray;
            push @out, $item;
        }
    }
    @out;
}

BEGIN {
    no strict 'refs';
    # die on methods which did not get extended by the specific feed
    foreach my $stub (qw/ add_item channel channel_class clear_item copyright xyz
        description docroot get get_item image init_feed item_class language
        limit_item link pubDate remove_item set sort_item test_feed title uniq_item
        / ) {
        *$stub = sub { croak +(ref $_[0])."->$stub() is not available" }
    }
}

# ----------------------------------------------------------------
package XML::FeedPP::Plugin;

sub run {
    my ($thing, $feed) = @_;
    my $class = ref $thing || $thing;
    Carp::croak "$class->run() is not implemented";
}

# ----------------------------------------------------------------
package XML::FeedPP::Item;
our @ISA = 'XML::FeedPP::Element';

*get_pubDate_w3cdtf  = \&XML::FeedPP::get_pubDate_w3cdtf;   # import
*get_pubDate_rfc1123 = \&XML::FeedPP::get_pubDate_rfc1123;
*get_pubDate_epoch   = \&XML::FeedPP::get_pubDate_epoch;

sub elements {
    my ($self, @pairs) = @_;
    while(@pairs) {
        my ($key, $val) = (shift @pairs, shift @pairs);
        if ($IS_ITEM_METHOD{$key} ) {
            $self->$key($val);
        }
        else {
            $self->set($key, $val);
        }
    }
}

# ----------------------------------------------------------------
package XML::FeedPP::RSS;
our @ISA = 'XML::FeedPP';

use Scalar::Util qw(reftype);

sub channel_class {
    'XML::FeedPP::RSS::Channel';
}

sub item_class {
    'XML::FeedPP::RSS::Item';
}

sub test_feed {
    my $thing  = shift;
    my $source = shift || $thing;
    ref $source && ref $source->{rss} ? __PACKAGE__ : undef;
}

sub init_feed {
    my $self = shift or return;

    my $rss = $self->{rss} ||= {};
    reftype $rss eq 'HASH'
        or Carp::croak "Invalid RSS 2.0 feed format: $rss";

    $rss->{-version} ||= $XML::FeedPP::RSS20_VERSION;

    my $channel = $rss->{channel} ||= $self->channel_class->new;
    $self->channel_class->ref_bless($channel);

    my $items = $channel->{item} || [];
    my @items = grep ref $_,    # skip empty items
        reftype $items eq 'HASH' ? $items : @$items;

    $self->item_class->ref_bless($_) for @items;
    $channel->{item} = \@items;

    $self;
}

sub merge_native_channel {
    my $self = shift;
    my $tree = shift or return;

    XML::FeedPP::Util::merge_hash($self->rss,     $tree->rss,     'channel');
    XML::FeedPP::Util::merge_hash($self->channel, $tree->channel, 'item');
}

sub add_item {
    my $self = shift;
    my( $init, $link, @rest ) = &XML::FeedPP::Util::param_even_odd(@_);

    ref $init || $link or Carp::croak "add_item needs an argument";
    if(ref $link) {
        return $self->add_clone_item( $link );
    }

    my $item = XML::FeedPP::RSS::Item->new(@rest);
    $item->link($link) if $link;
    $item->elements(@$init) if ref $init;
    push @{$self->channel->{item}}, $item;

    $item;
}

sub clear_item {
    my $self = shift;
    $self->channel->{item} = [];
}

sub remove_item {
    my ($self, $remove) = @_;
    my $list   = $self->channel->{item} or return;

    my @deleted;
    if ( $remove =~ /^-?\d+/ ) {
        @deleted = splice @$list, $remove, 1;
    }
    else {
        @deleted = grep $_->link eq $remove, @$list;
        @$list   = grep $_->link ne $remove, @$list;
    }

    wantarray ? @deleted : $deleted[0];
}

sub get_item {
    my ($self, $num) = @_;
    my $items = $self->channel->{item} ||= [];
    defined $num ? $items->[$num] : @$items;
}

sub sort_item {
    my $self   = shift;
    my $list   = $self->channel->{item} or return;

    my @epoch  = map { $_->get_pubDate_epoch() || 0 } @$list;
    my @sorted = map $list->[$_], sort { $epoch[$b] <=> $epoch[$a] } 0 .. $#$list;
    @$list = @sorted;
    scalar @$list;
}

sub uniq_item {
    my $self  = shift;
    my $list  = $self->channel->{item} or return;
    my (%check, @uniq);
    foreach my $item (@$list) {
        my $key = $item->guid || $item->link;
        push @uniq, $item unless $check{$key}++;
    }
    @$list = @uniq;
    scalar @$list;
}

sub limit_item {
    my $self  = shift;
    my $limit = shift;
    my $list  = $self->channel->{item} or return;
    if ( $limit > 0 && $limit < @$list ) {
        @$list = splice @$list, 0, $limit;   # remove from end
    }
    elsif ( $limit < 0 && -$limit < @$list ) {
        @$list = splice @$list, $limit;      # remove from start
    }
    scalar @$list;
}

sub docroot      { shift->{rss} }
sub channel      { shift->{rss}->{channel} }
sub set          { shift->channel->set(@_) }
sub get          { shift->channel->get(@_) }

sub title       { shift->channel->get_or_set(title =>       @_ ) }
sub description { shift->channel->get_or_set(description => @_ ) }
sub link        { shift->channel->get_or_set(link =>        @_ ) }
sub language    { shift->channel->get_or_set(language =>    @_ ) }
sub copyright   { shift->channel->get_or_set(copyright =>   @_ ) }

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;

    $self->channel->set_value(pubDate => XML::FeedPP::Util::get_rfc1123($date));
}

sub get_pubDate_native {
    my $channel = shift->channel;
       $channel->get_value("pubDate")    # normal RSS 2.0
    || $channel->get_value("dc:date");   # strange
}

sub image {
    my ($self, $url) = (shift, shift);
    my $channel = $self->channel;

    if (defined $url) {
        my ( $title, $link, $desc, $width, $height ) = @_;
        my $image = $channel->{image} ||= {};
        $image->{url}         = $url;
        $image->{title}       = $title  if defined $title;
        $image->{link}        = $link   if defined $link;
        $image->{description} = $desc   if defined $desc;
        $image->{width}       = $width  if defined $width;
        $image->{height}      = $height if defined $height;
        return $url;
    }

    my $image = $channel->{image}
       or return;

    wantarray
      ? @{$image}{ qw(url title link description width height) }
      : $image->{url};
}

# ----------------------------------------------------------------
package XML::FeedPP::RSS::Channel;
our @ISA = qw( XML::FeedPP::Element );

# ----------------------------------------------------------------
package XML::FeedPP::RSS::Item;
our @ISA = 'XML::FeedPP::Item';

sub title       { shift->get_or_set(title       => @_ ) }
sub description { shift->get_or_set(description => @_ ) }
sub category    { shift->get_set_array(category => @_ ) }

sub author {
    my $self = shift;
    return $self->set_value(author => @_) if @_;

    $self->get_value('author') || $self->get_value('dc:creator');
}

sub link {
    my ($self, $link) = @_;
    defined $link
        or return $self->get_value("link");

    defined $self->guid
        or $self->guid($link);

    $self->set_value(link => $link);
}

sub guid {
    my ($self, $guid, @args) = @_;
    defined $guid
        or return $self->get_value("guid");

    unshift @args, 'isPermaLink'   # XML::FeedPP 0.36's compat
        if @args == 1;

    @args or push @args, isPermaLink => 'true';
    $self->set_value(guid => $guid, @args);
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;

    $self->set_value(pubDate => XML::FeedPP::Util::get_rfc1123($date));
}

sub get_pubDate_native {
    my $self = shift;
    $self->get_value("pubDate")         # normal RSS 2.0
    || $self->get_value("dc:date");     # strange
}

sub image {
    my ($self, $url) = (shift, shift);

    if(defined $url) {
        my ($title, $link, $descr, $width, $height) = @_;
        my $image = $self->{image} ||= {};
        $image->{url}         = $url;
        $image->{title}       = $title  if defined $title;
        $image->{link}        = $link   if defined $link;
        $image->{description} = $descr  if defined $descr;
        $image->{width}       = $width  if defined $width;
        $image->{height}      = $height if defined $height;
        return undef;
    }

    my $image = $self->{image}
        or return;

    wantarray
      ? @{$image}{ qw(url title link description width height) }
      : $image->{url};
}

# ----------------------------------------------------------------
package XML::FeedPP::RDF;
our @ISA = 'XML::FeedPP';

use Scalar::Util qw(reftype);

sub channel_class {
    'XML::FeedPP::RDF::Channel';
}

sub item_class {
    'XML::FeedPP::RDF::Item';
}

sub test_feed {
    my $thing  = shift;
    my $source = shift || $thing;
    ref $source && ref $source->{'rdf:RDF'} ? __PACKAGE__ : undef;
}

sub init_feed {
    my $self = shift or return;

    $self->{'rdf:RDF'} ||= {};
    reftype $self->{'rdf:RDF'} eq 'HASH'
        or Carp::croak "Invalid RDF 1.0 feed format: $self->{'rdf:RDF'}";

    $self->xmlns('xmlns'     => $XML::FeedPP::XMLNS_RSS );
    $self->xmlns('xmlns:rdf' => $XML::FeedPP::XMLNS_RDF );
    $self->xmlns('xmlns:dc'  => $XML::FeedPP::XMLNS_DC );

    my $channel = $self->{'rdf:RDF'}->{channel} ||= $self->channel_class->new;
    $self->channel_class->ref_bless($channel);

    my $items   = $channel->{items}   ||= {};
    my $rdfseq  = $items->{'rdf:Seq'} ||= {};

    # http://www.kawa.net/works/perl/feedpp/feedpp.html#com-2008-05-17T13:13:33Z
    if (reftype $rdfseq eq 'ARRAY') {
        my $num1 = @$rdfseq;
        my $num2 = grep ref $_ && ref $_->{'rdf:li'}, @$rdfseq;
        my $num3 = grep ref $_ && keys %$_ == 1, @$rdfseq;
        if ( $num1 && $num1 == $num2 && $num1 == $num3 ) {
            my @newli = map @{$_->{'rdf:li'} || []}, @$rdfseq;
            $rdfseq = $items->{'rdf:Seq'} = { 'rdf:li' => \@newli };
        }
    }

    my $li = $rdfseq->{'rdf:li'} ||= [];
    $rdfseq->{'rdf:li'} = [ $li ]
        if reftype $li eq 'HASH';

    my $top_items = $self->{'rdf:RDF'}{item} ||= [];
    $top_items = $self->{'rdf:RDF'}{item} = [ $top_items ]
        if reftype $top_items eq 'HASH';

    $self->item_class->ref_bless($_) for @$top_items;
    $self;
}

sub merge_native_channel {
    my $self = shift;
    my $tree = shift or return;

    my $rdf1 = $self->{'rdf:RDF'};
    my $rdf2 = $tree->{'rdf:RDF'};
    XML::FeedPP::Util::merge_hash($rdf1, $rdf2, qw(channel item));
    XML::FeedPP::Util::merge_hash($rdf1->{channel}, $rdf2->{channel}, 'items');
}

sub add_item {
    my $self = shift;
    my( $init, $link, @rest ) = &XML::FeedPP::Util::param_even_odd(@_);

    ref $init || $link
        or Carp::croak "add_item needs an argument";

    if ( ref $link ) {
        return $self->add_clone_item( $link );
    }

    my $rdfli = $self->item_class->new;
    $rdfli->{'-rdf:resource'} = $link;

    push @{$self->channel->{items}{'rdf:Seq'}{'rdf:li'}}, $rdfli;

    my $item = XML::FeedPP::RDF::Item->new(@rest);
    $item->link($link) if $link;
    $item->elements(@$init) if ref $init;
    push @{ $self->{'rdf:RDF'}->{item} }, $item;

    $item;
}

sub clear_item {
    my $self = shift;
    $self->{'rdf:RDF'}->{item} = [];
    $self->__refresh_items;
}

sub remove_item {
    my ($self, $remove) = @_;
    my $list   = $self->{'rdf:RDF'}->{item} or return;

    my @deleted;
    if ( $remove =~ /^-?\d+/ ) {
        @deleted = splice @$list, $remove, 1;
    }
    else {
        @deleted = grep $_->link eq $remove, @$list;
        @$list   = grep $_->link ne $remove, @$list;
    }

    $self->__refresh_items();
    wantarray ? @deleted : $deleted[0];
}

sub get_item {
    my ($self, $num) = @_;
    my $items = $self->{'rdf:RDF'}->{item} ||= [];
    defined $num ? $items->[$num] : @$items;
}

sub sort_item {
    my $self = shift;
    my $list = $self->{'rdf:RDF'}->{item} or return;

    my @epoch  = map { $_->get_pubDate_epoch() || 0 } @$list;
    my @sorted = map $list->[$_], sort { $epoch[$b] <=> $epoch[$a] } 0 .. $#$list;
    @$list = @sorted;
    $self->__refresh_items;
}

sub uniq_item {
    my $self  = shift;
    my $list  = $self->{'rdf:RDF'}->{item} or return;

    my (%check, @uniq);
    foreach my $item (@$list) {
        my $link = $item->link;
        push @uniq, $item unless $check{$link}++;
    }
    $self->{'rdf:RDF'}->{item} = \@uniq;
    $self->__refresh_items;
}

sub limit_item {
    my $self  = shift;
    my $limit = shift;
    my $list  = $self->{'rdf:RDF'}->{item} or return;
    if ( $limit > 0 && $limit < @$list ) {
        @$list = splice @$list, 0, $limit;   # remove from end
    }
    elsif ( $limit < 0 && -$limit < @$list ) {
        @$list = splice @$list, $limit;      # remove from start
    }
    $self->__refresh_items;
}

sub __refresh_items {
    my $self = shift;
    my $list = $self->{'rdf:RDF'}{item} or return;

    my $dest = $self->channel->{items}{'rdf:Seq'}{'rdf:li'} = [];
    foreach my $item (@$list) {
        my $rdfli = XML::FeedPP::Element->new;
        $rdfli->{'-rdf:resource'} = $item->link;
        push @$dest, $rdfli;
    }
    scalar @$dest;
}

sub docroot     { shift->{'rdf:RDF'}; }
sub channel     { shift->docroot->{channel} }
sub set         { shift->channel->set(@_); }
sub get         { shift->channel->get(@_); }
sub title       { shift->channel->get_or_set(title         => @_ ) }
sub description { shift->channel->get_or_set(description   => @_ ) }
sub language    { shift->channel->get_or_set('dc:language' => @_ ) }
sub copyright   { shift->channel->get_or_set('dc:rights'   => @_ ) }

sub link {
    my ($self, $link) = (shift, shift);
    my $channel = $self->channel;

    defined $link
        or return $channel->get_value("link");

    $channel->{'-rdf:about'} = $link;
    $channel->set_value(link => $link, @_);
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;
    $self->channel->set_value("dc:date" => XML::FeedPP::Util::get_w3cdtf($date));
}

sub get_pubDate_native {
    shift->channel->get_value("dc:date");
}

*get_pubDate_w3cdtf = \&get_pubDate_native;

sub image {
    my ($self, $url, $title, $link) = @_;

    if (defined $url) {
        my $chan_image = $self->channel->{image} ||= {};
        $chan_image->{'-rdf:resource'} = $url;

        my $image = $self->{'rdf:RDF'}->{image} ||= {};
        $image->{'-rdf:about'} = $url;
        $image->{url}   = $url;
        $image->{title} = $title if defined $title;
        $image->{link}  = $link  if defined $link;
        return undef;
    }

    if(my $image = $self->{'rdf:RDF'}->{image} ) {
        return wantarray
          ? @{$image}{ qw/url title link/ }
          : $image->{url};
    }

    if(my $image = $self->channel->{image}) {
        return $image->{'-rdf:resource'};
    }

    undef;
}

# ----------------------------------------------------------------
package XML::FeedPP::RDF::Channel;
our @ISA = 'XML::FeedPP::Element';

# ----------------------------------------------------------------
package XML::FeedPP::RDF::Item;
our @ISA = 'XML::FeedPP::Item';

sub title       { shift->get_or_set( "title",       @_ ); }
sub description { shift->get_or_set( "description", @_ ); }
sub category    { shift->get_set_array( "dc:subject",  @_ ); }
sub guid { undef; }    # this element is NOT supported for RDF

sub author {
    my ($self, $author) = @_;
    defined $author
        or return $self->get_value('dc:creator') || $self->get_value('creator');

    $self->set_value('dc:creator' => $author);
}

sub link {
    my ($self, $link) = (shift, shift);
    defined $link or return $self->get_value("link");

    $self->{'-rdf:about'} = $link;
    $self->set_value(link => $link, @_);
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;

    $self->set_value("dc:date" => XML::FeedPP::Util::get_w3cdtf($date));
}

sub get_pubDate_native {
    shift->get_value("dc:date");
}

*get_pubDate_w3cdtf = \&get_pubDate_native;

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Common;
our @ISA = 'XML::FeedPP';

use Scalar::Util qw(reftype);

sub merge_native_channel {
    my $self = shift;
    my $tree = shift or return;
    XML::FeedPP::Util::merge_hash( $self->{feed}, $tree->{feed}, 'entry');
}

sub add_item {
    my $self = shift;
    my ($init, $link, @rest) = XML::FeedPP::Util::param_even_odd(@_);

    ref $init || $link
         or Carp::croak "add_item needs an argument";

    if ( ref $link ) {
        return $self->add_clone_item( $link );
    }

    my $item = $self->item_class->new(@rest);
    $item->link($link) if $link;
    $item->elements(@$init) if ref $init;
    push @{ $self->{feed}->{entry} }, $item;

    $item;
}

sub clear_item {
    my $self = shift;
    $self->{feed}->{entry} = [];
}

sub remove_item {
    my ($self, $remove) = @_;
    my $list   = $self->{feed}{entry} or return;

    my @deleted;
    if ( $remove =~ /^-?\d+/ ) {
        @deleted = splice @$list, $remove, 1;
    }
    else {
        @deleted = grep $_->link eq $remove, @$list;
        @$list   = grep $_->link ne $remove, @$list;
    }

    wantarray ? @deleted : $deleted[0];
}

sub get_item {
    my ($self, $num) = @_;
    my $entries = $self->{feed}{entry} ||= [];
    defined $num ? $entries->[$num] : @$entries;
}

sub sort_item {
    my $self = shift;
    my $list = $self->{feed}->{entry} or return;

    my @epoch  = map { $_->get_pubDate_epoch() || 0 } @$list;
    my @sorted = map $list->[$_], sort { $epoch[$b] <=> $epoch[$a] } 0 .. $#$list;
    @$list = @sorted;
    scalar @$list;
}

sub uniq_item {
    my $self  = shift;
    my $list  = $self->{feed}{entry} or return;
    my (%check, @uniq);
    foreach my $item (@$list) {
        my $link = $item->guid;
        push @uniq, $item unless $check{$link}++;
    }
    @$list = @uniq;
}

sub limit_item {
    my ($self, $limit) = @_;
    my $list  = $self->{feed}->{entry} or return;
    if ( $limit > 0 && $limit < @$list ) {
        @$list = splice @$list, 0, $limit;   # remove from end
    }
    elsif ( $limit < 0 && -$limit < @$list ) {
        @$list = splice @$list, $limit;      # remove from start
    }
    scalar @$list;
}

sub docroot { shift->{feed}; }
sub channel { shift->{feed}; }
sub set     { shift->{feed}->set(@_); }
sub get     { shift->{feed}->get(@_); }

sub language {
    my ($self, $lang) = @_;
    defined $lang or return $self->{feed}->{'-xml:lang'};
    $self->{feed}->{'-xml:lang'} = $lang;
}

sub image {
    my ($self, $href, $title) = @_;

    my $link   = $self->{feed}{link} || [];
    my @links  = reftype $link eq 'HASH' ? $link : @$link;
    (my $icon) = grep ref $_ && defined $_->{-rel} && $_->{-rel} eq 'icon',
        @links;

    if (defined $href) {
        my $rext = join( "|", map {"\Q$_\E"} keys %$XML::FeedPP::MIME_TYPES );
        my $ext  = ( $href =~ m#[^/]\.($rext)(\W|$)#i )[0];
        my $type = $XML::FeedPP::MIME_TYPES->{$ext} if $ext;

        if ($icon) {
            # Change icon
            $icon->{-href}   = $href;
            $icon->{-type}   = $type if $type;
            $icon->{-title}  = $title if $title;
        }
        else {
            # Create first icon
            my %newicon;
            $newicon{-rel}   = 'icon';
            $newicon{-href}  = $href;
            $newicon{-type}  = $type  if $type;
            $newicon{-title} = $title if $title;

            my $flink = $self->{feed}{link} ||= [];
            if (reftype $flink eq 'HASH') {
                $self->{feed}{link} = [ $flink, \%newicon ];
            }
            else {
                push @$flink, \%newicon;
            }
        }
        return;
    }

    if ($icon) {
        return $icon->{-href} unless wantarray;
        return ($icon->{-href}, $icon->{-title});
    }

    undef;
}

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Atom03;
our @ISA = 'XML::FeedPP::Atom::Common';

use Scalar::Util  qw(reftype);

sub channel_class {
    'XML::FeedPP::Atom::Atom03::Feed';
}

sub item_class {
    'XML::FeedPP::Atom::Atom03::Entry';
}

sub test_feed {
    my $thing  = shift;
    my $source = shift || $thing;

    ref $source && ref $source->{feed}
        or return;

    my $xmlns = $source->{feed}{-xmlns} || '';
    $xmlns eq $XML::FeedPP::XMLNS_ATOM03 ? __PACKAGE__ : undef;
}

sub init_feed {
    my $self = shift or return;

    my $feed = $self->{feed} ||= $self->channel_class->new;
    reftype $feed eq 'HASH'
        or Carp::croak "Invalid Atom 0.3 feed format: $feed";

    $self->channel_class->ref_bless($feed);
    $self->xmlns(xmlns => $XML::FeedPP::XMLNS_ATOM03);
    $feed->{-version} ||= $XML::FeedPP::ATOM03_VERSION;

    my $items = $feed->{entry} ||= [];
    if (reftype $items eq 'HASH') {
        $items = $feed->{entry} = [ $items ];
    }
    $self->item_class->ref_bless($_) for @$items;
    $feed->{author} ||= { name => '' };    # dummy for validation
    $self;
}

sub title {
    my ($self, $title) = @_;
    defined $title or return $self->{feed}->get_value('title');
    $self->{feed}->set_value(title => $title, type => 'text/plain');
}

sub description {
    my ($self, $descr) = @_;
    my $feed = $self->{feed};
    defined $descr
        or return $feed->get_value('tagline') || $feed->get_value('subtitle');

    $feed->set_value(tagline => $descr, type => 'text/html', mode => 'escaped');
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;

    $self->{feed}->set_value(modified => XML::FeedPP::Util::get_w3cdtf($date));
}

sub get_pubDate_native {
    my $feed = shift->{feed};
       $feed->get_value('modified')     # Atom 0.3
    || $feed->get_value('updated');     # Atom 1.0
}

*get_pubDate_w3cdtf = \&get_pubDate_native;

sub copyright {
    my ($self, $copy) = @_;
    my $feed = $self->{feed};
    defined $copy
        or return $feed->get_value('copyright') || $feed->get_value('rights');

    $feed->set_value(copyright => $copy);
}

sub link {
    my ($self, $href) = @_;

    my $link = $self->{feed}->{link} || [];
    $link = [ $link ] if reftype $link eq 'HASH';
    $link = [ grep ref $_, @$link ];
    $link = [ grep {
        ! exists $_->{'-rel'} || $_->{'-rel'} eq 'alternate'
    } @$link ];
    $link = [ grep {
        ! exists $_->{'-type'} || $_->{'-type'} =~ m#^text/(x-)?html#i
    } @$link ];
    my $html = shift @$link;

    if ( defined $href ) {
        if ( ref $html ) {
            $html->{'-href'} = $href;
        }
        else {
            my $hash = {
                -rel    =>  'alternate',
                -type   =>  'text/html',
                -href   =>  $href,
            };
            my $flink = $self->{feed}->{link} ||= [];
            if (reftype $flink eq 'HASH') {
                $self->{feed}{link} = [ $flink, $hash ];
            }
            else {
                push @$flink, $hash;
            }
        }
        return;
    }

    ref $html ? $html->{-href} : undef;
}

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Atom10;
our @ISA = 'XML::FeedPP::Atom::Common';

use Scalar::Util  qw(reftype);

sub channel_class {
    'XML::FeedPP::Atom::Atom10::Feed';
}

sub item_class {
    'XML::FeedPP::Atom::Atom10::Entry';
}

sub test_feed {
    my $thing  = shift;
    my $source = shift || $thing;

    ref $source && ref $source->{feed}
        or return;

    my $xmlns = $source->{feed}{-xmlns} || '';
    $xmlns eq $XML::FeedPP::XMLNS_ATOM10 ? __PACKAGE__ : undef;
}

sub init_feed {
    my $self = shift or return;

    my $feed = $self->{feed} ||= $self->channel_class->new;
    reftype $feed eq 'HASH'
        or Carp::croak "Invalid Atom 1.0 feed format: $feed";

    $self->channel_class->ref_bless($feed);
    $self->xmlns(xmlns => $XML::FeedPP::XMLNS_ATOM10 );
#   $feed->{-version} ||= $XML::FeedPP::ATOM10_VERSION;

    my $items = $feed->{entry} ||= [];
    if (reftype $items eq 'HASH') {
        $items = $feed->{entry} = [ $items ];
    }
    $self->item_class->ref_bless($_) for @$items;;
    $self;
}

sub title {
    my ($self, $title) = (shift, shift);
    defined $title or return $self->{feed}->get_value('title');
    $self->{feed}->set_value(title => $title, @_);
}

sub description {
    my ($self, $descr) = (shift, shift);
    my $feed = $self->{feed};

    defined $descr
        or return $feed->get_value('content')  || $feed->get_value('summary')
               || $feed->get_value('subtitle') || $feed->get_value('tagline');

    $feed->set_value(content => $descr, @_);     # type => 'text'
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;
    $self->{feed}->set_value(updated => XML::FeedPP::Util::get_w3cdtf($date));
}

sub get_pubDate_native {
    my $feed = shift->{feed};

    # Atom 1.0 resp 0.3
    $feed->get_value('updated') || $feed->get_value('modified');
}

*get_pubDate_w3cdtf = \&get_pubDate_native;

sub copyright {
    my $feed = shift->{feed};
    my $copy = shift;

    defined $copy
        or return $feed->get_value('rights') || $feed->get_value('copyright');

    $feed->set_value(rights => $copy);
}

sub link {
    my $self = shift;
    my $href = shift;

    my $link = $self->{feed}->{link} || [];
    $link = [$link] if reftype $link eq 'HASH';
    $link = [ grep { ref $_ } @$link ];
    $link = [ grep {
        ! exists $_->{'-rel'} || $_->{'-rel'} eq 'alternate'
    } @$link ];
    my $html = shift @$link;

    if ( defined $href ) {
        if (ref $html) {
            $html->{-href} = $href;
        }
        else {
            my $hash = {
                -rel    =>  'alternate',
                -href   =>  $href,
            };
            my $flink = $self->{feed}{link} ||= [];
            if (reftype $flink eq 'HASH') {
                $self->{feed}{link} = [ $flink, $hash ];
            }
            else {
                push @$flink, $hash;
            }
        }
        return;
    }

    ref $html ? $html->{-href} : undef;
}

# ----------------------------------------------------------------
package XML::FeedPP::Atom;
our @ISA = 'XML::FeedPP::Atom::Atom03';

sub test_feed {
    my $thing  = shift;
    my $source = shift || $thing;
    ref $source && ref $source->{feed} ? __PACKAGE__ : undef;
}

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Common::Feed;
our @ISA = 'XML::FeedPP::Element';

use Scalar::Util  qw(reftype);

# <content type="xhtml"><div>...</div></content>
# http://www.ietf.org/rfc/rfc4287.txt
# 3. If the value of "type" is "xhtml", the content of atom:content
#    MUST be a single XHTML div element [XHTML] and SHOULD be suitable
#    for handling as XHTML. The XHTML div element itself MUST NOT be
#    considered part of the content.

sub _fetch_value {
    my $self  = shift;
    my $value = shift;

    if (reftype $value eq 'HASH'
        && exists $value->{'-type'}
        && ($value->{'-type'} eq "xhtml")) {
        my $child = [ grep { /^[^\-\#]/ } keys %$value ];
        if (@$child == 1) {
            my $div = shift @$child;
            if ($div =~ /^([^:]+:)?div$/i) {
                return $value->{$div};
            }
        }
    }

    $self->SUPER::_fetch_value($value);
}

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Atom03::Feed;
our @ISA = 'XML::FeedPP::Atom::Common::Feed';

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Atom10::Feed;
our @ISA = 'XML::FeedPP::Atom::Common::Feed';

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Common::Entry;
our @ISA = 'XML::FeedPP::Item';

sub author {
    my ($self, $name) = @_;
    unless (defined $name) {
        my $author = $self->{author}{name} if ref $self->{author};
        return $author;
    }
    my $author = ref $name ? $name : { name => $name };
    $self->{author} = $author;
}

sub guid { shift->get_or_set( 'id', @_ ); }

*_fetch_value = \&XML::FeedPP::Atom::Common::Feed::_fetch_value;

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Atom03::Entry;
our @ISA = 'XML::FeedPP::Atom::Common::Entry';

use Scalar::Util  qw(reftype);

sub description {
    my ($self, $descr) = @_;
    defined $descr
        or return $self->get_value('content') || $self->get_value('summary');

    $self->set_value(
        content => $descr,
        type    => 'text/html',
        mode    => 'escaped',
    );
}

sub link {
    my ($self, $href) = @_;

    my $link = $self->{link} || [];
    $link = [$link] if reftype $link eq 'HASH';
    $link = [ grep { ref $_ } @$link ];
    $link = [ grep {
        ! exists $_->{'-rel'} || $_->{'-rel'} eq 'alternate'
    } @$link ];
    $link = [ grep {
        ! exists $_->{'-type'} || $_->{'-type'} =~ m#^text/(x-)?html#i
    } @$link ];
    my $html = shift @$link;

    if ( defined $href ) {
        if ( ref $html ) {
            $html->{'-href'} = $href;
        }
        else {
            my $hash = {
                -rel  => 'alternate',
                -type => 'text/html',
                -href => $href,
            };

            my $flink = $self->{link} ||= [];
            if (reftype $flink eq 'HASH') {
                $self->{link} = [ $flink, $hash ];
            }
            else {
                push @$flink, $hash;
            }
        }
        defined $self->guid or $self->guid($href);
        return;
    }

    ref $html ? $html->{-href} : undef;
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date
        or return $self->get_pubDate_w3cdtf;

    my $fmt = XML::FeedPP::Util::get_w3cdtf($date);
    $self->set_value(issued =>   $fmt);
    $self->set_value(modified => $fmt);
}

sub get_pubDate_native {
    my $self = shift;
       $self->get_value('modified')     # Atom 0.3
    || $self->get_value('issued')       # Atom 0.3
    || $self->get_value('updated')      # Atom 1.0
    || $self->get_value('published');   # Atom 1.0
}

*get_pubDate_w3cdtf = \&get_pubDate_native;

sub title {
    my ($self, $title) = @_;
    defined $title or return $self->get_value('title');
    $self->set_value(title => $title, type => 'text/plain');
}

sub category { undef }    # this element is NOT supported for Atom 0.3

# ----------------------------------------------------------------
package XML::FeedPP::Atom::Atom10::Entry;
our @ISA = 'XML::FeedPP::Atom::Common::Entry';

use Scalar::Util  qw(reftype);

sub description {
    my ($self, $descr) = @_;
    defined $descr
        or return $self->get_value('content') || $self->get_value('summary');

    $self->set_value(content => $descr, @_ );
}

sub link {
    my ($self, $href) = @_;

    my $link  = $self->{link} || [];
    my @links = reftype $link eq 'HASH' ? $link : @$link;
    (my $html) = grep ! exists $_->{-rel} || $_->{-rel} eq 'alternate',
        grep ref, @links;

    if (defined $href) {
        if (ref $html) { # Change
            $html->{-href} = $href;
        }
        else { # Add
            my $add   = { -href   =>  $href };
            my $flink = $self->{link} ||= [];
            if (reftype $flink eq 'HASH') {
                $self->{link} = [ $flink, $add ];
            }
            else {
                push @$flink, $add;
            }
        }

        defined $self->guid
            or $self->guid($href);

        return;
    }

    if (ref $html) {
        return $html->{'-href'};
    }

    undef;
}

sub pubDate {
    my ($self, $date) = @_;
    defined $date or return $self->get_pubDate_w3cdtf;
    $self->set_value(updated => XML::FeedPP::Util::get_w3cdtf($date));
}

sub get_pubDate_native {
    my $self = shift;
       $self->get_value('updated')      # Atom 1.0
    || $self->get_value('published')    # Atom 1.0
    || $self->get_value('issued')       # Atom 0.3
    || $self->get_value('modified');    # Atom 0.3
}

*get_pubDate_w3cdtf = \&get_pubDate_native;

sub title {
    my ($self, $title, $type) = @_;
    defined $title or return $self->get_value('title');
    $self->set_value(title => $title, type => $type || 'text');
}

sub category {
    my $self = shift;
    if ( @_ ) {
        my @cats = ref $_[0] ? @{$_[0]} : @_;
        my @list = map +{-term => $_}, @cats;
        $self->{category} = @list > 1 ? \@list : $list[0];
        return;
    }

    $self->{category}
        or return;

     my $list = $self->{category} || [];
     my @list = reftype $list eq 'ARRAY' ? @$list : $list;
     my @term = map { ref $_ && $_->{-term} } @list;

     #XXX inconsequent: all other method return list in LIST context
     @term > 1 ? \@term : $term[0];
}

# ----------------------------------------------------------------
package XML::FeedPP::Element;

use Scalar::Util  qw(reftype);

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

sub ref_bless {
    my ($class, $hash) = @_;
    bless $hash, $class;
}

sub set {
    my $self = shift;

    while (@_) {
        my ($key, $val) = (shift, shift);
        my $node = $self;
        while ( $key =~ s#^([^/]+)/##s ) {
            my $child = $1;
            if ( ref $node->{$child} ) {
                # ok
            }
            elsif ( defined $node->{$child} ) {
                $node->{$child} = { '#text' => $node->{$child} };
            }
            else {
                $node->{$child} = {};
            }
            $node = $node->{$child};
        }

        my ($tagname, $attr) = split /\@/, $key, 2;
        if ( $tagname eq "" && defined $attr ) {
            $node->{"-$attr"} = $val;
        }
        elsif ( defined $attr ) {
            if (reftype $node->{$tagname} eq 'ARRAY') {
                $node->{$tagname} = shift @{$node->{$tagname}};
            }
            my $hkey = "-$attr";
            if ( ref $node->{$tagname} ) {
                $node->{$tagname}->{$hkey} = $val;
            }
            elsif ( defined $node->{$tagname} ) {
                $node->{$tagname} = {
                    '#text' => $node->{$tagname},
                    $hkey   => $val,
                };
            }
            else {
                $node->{$tagname} = { $hkey => $val };
            }
        }
        elsif ( defined $tagname ) {
            if (reftype $node->{$tagname} eq 'ARRAY') {
                $node->{$tagname} = shift @{$node->{$tagname}};
            }
            if ( ref $node->{$tagname} ) {
                $node->{$tagname}->{'#text'} = $val;
            }
            else {
                $node->{$tagname} = $val;
            }
        }
    }
}

sub get {
    my ($self, $key) = @_;
    my $node = $self;

    while ( $key =~ s#^([^/]+)/##s ) {
        my $child = $1;
        ref $node && exists $node->{$child} or return;
        $node = $node->{$child};
    }

    my ($tagname, $attr) = split /\@/, $key, 2;
    ref $node or return;

    # return unless exists $node->{$tagname};
    if ( $tagname eq "" && defined $attr ) {    # @attribute
        return $node->{"-$attr"};
    }

    if ( defined $attr ) {                   # node@attribute
        ref $node->{$tagname} or return;
        my $hkey = "-$attr";
        if (reftype $node->{$tagname} eq 'ARRAY') {
            my @list = map { ref $_ ? $_->{$hkey} : undef } @{$node->{$tagname}};
            return @list if wantarray;
            return ( grep defined, @list )[0];
        }
        return $node->{$tagname}->{$hkey};
    }

    ref $node->{$tagname}
        or return $node->{$tagname};

    if (reftype $node->{$tagname} eq 'ARRAY') {
         my @list = map { ref $_ ? $_->{'#text'} : $_ } @{$node->{$tagname}};
         return @list if wantarray;
         return (grep defined, @list)[0];
    }

    return $node->{$tagname}->{'#text'};
}

sub get_set_array {
    my $self = shift;
    my $elem = shift;
    my $value = shift;
    if ( ref $value ) {
        $self->{$elem} = $value;
    } elsif ( defined $value ) {
        $value = [ $value, @_ ] if @_;
        $self->{$elem} = $value;
    } else {
        my @ret = $self->get_value($elem);
        return @ret > 1 ? \@ret : $ret[0];
    }
}

sub get_or_set {
    my ($self, $elem) = (shift, shift);
    return $self->set_value($elem, @_) if @_;

    $self->get_value($elem);
}

sub get_value {
    my ($self, $elem) = (shift, shift);
    unless(exists $self->{$elem}) {
        #XXX TreePP does not understand prefixes, which is usually not a
        # problem because most clients break on them... "best practices"
        # for feeds often explicitly state to use "the default" namespace.
        # Let's try to ignore the namespace here.
        ($elem) = grep /\:\Q$elem\E$/, keys %$self;
        defined $elem or return;
    }

    my $value = $self->{$elem};
    ref $value or return $value;

    # multiple elements
    if(reftype $value eq 'ARRAY') {
        return map $self->_fetch_value($_), @$value
            if wantarray;

        $value = $value->[0];
    }

    $self->_fetch_value($value);
}

sub _fetch_value {
    my $self  = shift;
    my $value = shift;

    if (reftype $value eq 'HASH') {
        # text node of an element with attributes
        return defined $value->{'#text'}
          ? $self->_fetch_value($value->{'#text'})
          : '';
    }

    if (reftype $value eq 'SCALAR') {
        # CDATA section as a scalar reference
        return $$value;
    }

    return $value;
}

sub set_value {
    my ($self, $elem, $text, @attr) = @_;

    my $node = $self->{$elem};
    if (reftype $node eq 'HASH') {
        $node->{'#text'} = $text;
    }
    else {
        $self->{$elem} = $text;
    }
    $self->set_attr($elem, @attr) if @attr;
    undef;
}

sub get_attr {
    my ($self, $elem, $key) = @_;
    my $item = $self->{$elem};
    $item && ref $item ? $item->{"-$key"} : undef;
}

sub set_attr {
    my ($self, $elem, @attr) = @_;
    my $item = $self->{$elem} ||= {};

    # convert to HASH, to be abe to store attribute
    $item = $self->{$elem} = { '#text' => $item }
        if defined $item && ! ref $item || ref $item eq 'SCALAR';

    while(@attr) {
        my ($key, $val) = (shift @attr, shift @attr);
        if(defined $val) {
            $self->{$elem}->{"-$key"} = $val;
        }
        else {
            delete $self->{$elem}->{"-$key"};
        }
    }

    undef;
}

# ----------------------------------------------------------------
package XML::FeedPP::Util;

my @DoW = qw(Sun Mon Tue Wed Thu Fri Sat);
my @MoY = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
my %MoY; @MoY{ map uc($_), @MoY } = ( 1 .. 12 );
my $tz_now = time();
my $tz_offset = Time::Local::timegm( localtime($tz_now) ) -
                Time::Local::timegm( gmtime($tz_now) );
my $tz_hour = int( $tz_offset / 3600 );
my $tz_min  = int( $tz_offset / 60 ) % 60;

my $rfc1123_regexp = qr{
    ^(?:[A-Za-z]+,\s*)? (\d+)\s+ ([A-Za-z]+)\s+ (\d+)\s+
    (\d+):(\d+)(?::(\d+)(?:\.\d*)?)?\s*
    ([\+\-]\d+:?\d{2} | [ECMP][DS]T )?
}xi;

my $w3cdtf_regexp = qr{
    ^(\d+)-(\d+)-(\d+)
    (?:T(\d+):(\d+)(?::(\d+)(?:\.\d*)?\:?)?\s*
    ([\+\-]\d+:?\d{2})?|$)
}x;

my %tzmap = qw(
    EDT -4  EST -5  CDT -5  CST -6
    MDT -6  MST -7  PDT -7  PST -8
);

sub epoch_to_w3cdtf {
    my $epoch = shift;
    defined $epoch or return;

    my ($sec, $min, $hour, $day, $mon, $year) = gmtime($epoch+$tz_offset);
    $year += 1900;
    $mon++;
    my $tz = $tz_offset ? sprintf('%+03d:%02d', $tz_hour, $tz_min) : 'Z';
    sprintf '%04d-%02d-%02dT%02d:%02d:%02d%s', $year, $mon, $day, $hour, $min, $sec, $tz;
}

sub epoch_to_rfc1123 {
    my $epoch = shift;
    defined $epoch or return;

    my ($sec, $min, $hour, $mday, $mon, $year, $wday) = gmtime($epoch+$tz_offset);
    $year += 1900;
    my $tz = $tz_offset ? sprintf( '%+03d%02d', $tz_hour, $tz_min ) : 'GMT';
    sprintf( '%s, %02d %s %04d %02d:%02d:%02d %s',
        $DoW[$wday], $mday, $MoY[$mon], $year, $hour, $min, $sec, $tz );
}

sub rfc1123_to_w3cdtf {
    my $str = shift;
    defined $str or return;

    my ( $mday, $mon, $year, $hour, $min, $sec, $tz ) = ( $str =~ $rfc1123_regexp );
    ($year && $mon && $mday) or return;

    $year += 2000 if $year < 77;
    $year += 1900 if $year < 100;

    $mon = $MoY{ uc($mon) } or return;
    if ( defined $tz && $tz ne '' && $tz ne 'GMT' ) {
        my $off = get_tz_offset($tz) / 60;
        $tz = sprintf( '%+03d:%02d', $off/60, $off%60 );
    }
    else {
        $tz = 'Z';
    }
    sprintf '%04d-%02d-%02dT%02d:%02d:%02d%s', $year, $mon, $mday, $hour, $min, $sec,$tz;
}

sub w3cdtf_to_rfc1123 {
    my $str = shift;
    defined $str or return;

    my ($year, $mon, $mday, $hour, $min, $sec, $tz) = $str =~ $w3cdtf_regexp;
    $year > 1900 && $mon && $mday or return;

    $hour ||= 0;
    $min  ||= 0;
    $sec  ||= 0;
    my $epoch = eval { Time::Local::timegm( $sec, $min, $hour, $mday, $mon-1, $year ) }
        or return;

    my $wday = ( gmtime($epoch) )[6];
    if ( defined $tz && $tz ne '' && $tz ne 'Z' ) {
        my $off = get_tz_offset($tz) / 60;
        $tz = sprintf( '%+03d%02d', $off/60, $off%60 );
    }
    else {
        $tz = 'GMT';
    }
    sprintf '%s, %02d %s %04d %02d:%02d:%02d %s',
        $DoW[$wday], $mday, $MoY[$mon - 1], $year, $hour, $min, $sec, $tz;
}

sub rfc1123_to_epoch {
    my $str = shift;
    defined $str or return;

    my ($mday, $mon, $year, $hour, $min, $sec, $tz) = $str =~ $rfc1123_regexp;
    $year && $mon && $mday or return;

    $year += 2000 if $year < 77;
    $year += 1900 if $year < 100;
    $mon   = $MoY{ uc($mon) } or return;

    my $epoch = eval { Time::Local::timegm($sec, $min, $hour, $mday, $mon-1, $year) }
        or return;
    $epoch -= get_tz_offset( $tz );
    $epoch;
}

sub w3cdtf_to_epoch {
    my $str = shift;
    defined $str or return;

    my ($year, $mon, $mday, $hour, $min, $sec, $tz ) = $str =~ $w3cdtf_regexp;
    $year > 1900 && $mon && $mday or return;

    $hour ||= 0;
    $min  ||= 0;
    $sec  ||= 0;
    my $epoch = eval { Time::Local::timegm($sec, $min, $hour, $mday, $mon-1, $year) }
        or return;

    $epoch -= get_tz_offset( $tz );
    $epoch;
}

sub get_tz_offset {
    my $tz = shift;
    defined $tz or return 0;

    return $tzmap{$tz}*60*60
        if exists $tzmap{$tz};

    if($tz =~ m/^([\+\-]?)(\d+):?(\d{2})$/) {
        my ($sign, $ho, $mi) = ($1, $2, $3);
        return ($ho * 60 + $mi) * ($sign eq '-' ? -60 : 60);
    }

    0;
}

sub get_w3cdtf {
    my $date = shift;
    defined $date or return;

      $date =~ /^[0-9]+$/s     ? epoch_to_w3cdtf($date)
    : $date =~ $rfc1123_regexp ? rfc1123_to_w3cdtf($date)
    : $date =~ $w3cdtf_regexp  ? $date
    : undef;
}

sub get_rfc1123 {
    my $date = shift;
    defined $date or return;

      $date =~ /^\d+$/s        ? epoch_to_rfc1123($date)
    : $date =~ $rfc1123_regexp ? $date
    : $date =~ $w3cdtf_regexp  ? w3cdtf_to_rfc1123($date)
    : undef;
}

sub get_epoch {
    my $date = shift;
    defined $date or return;

      $date =~ /^\d+$/s ? $date
    : $date =~ $rfc1123_regexp  ? rfc1123_to_epoch($date)
    : $date =~ $w3cdtf_regexp   ? w3cdtf_to_epoch($date)
    : undef;
}

sub merge_hash {
    my $base  = shift or return;
    my $merge = shift or return;

    my %exclude = map { $_ => 1 } @_;
    foreach my $key ( keys %$merge ) {
        next if $exclude{$key} || exists $base->{$key};
        $base->{$key} = $merge->{$key};
    }
}

sub param_even_odd {
    if ( @_ % 2 == 0 ) {
        # even num of args - new( key1 => val1, key2 => arg2 );
        my $array = [ @_ ];
        return $array;
    }
    else {
        # odd num of args - new( first, key1 => val1, key2 => arg2 );
        return ( undef, @_ );
    }
}

1;

# ----------------------------------------------------------------

__END__

=head1 NAME

XML::FeedPP -- Parse/write/merge/edit RSS/RDF/Atom syndication feeds

=head1 SYNOPSIS

Get an RSS file and parse it:

    use XML::FeedPP ();
    my $source = 'http://use.perl.org/index.rss';
    my $feed = XML::FeedPP->new( $source );
    print "Title: ", $feed->title(), "\n";
    print "Date: ", $feed->pubDate(), "\n";
    foreach my $item ( $feed->get_item() ) {
        print "URL: ", $item->link(), "\n";
        print "Title: ", $item->title(), "\n";
    }

Generate an RDF file and save it:

    use XML::FeedPP ();
    my $feed = XML::FeedPP::RDF->new();
    $feed->title( "use Perl" );
    $feed->link( "http://use.perl.org/" );
    $feed->pubDate( "Thu, 23 Feb 2006 14:43:43 +0900" );
    my $item = $feed->add_item( "http://search.cpan.org/~kawasaki/XML-TreePP-0.02" );
    $item->title( "Pure Perl implementation for parsing/writing xml file" );
    $item->pubDate( "2006-02-23T14:43:43+09:00" );
    $feed->to_file( "index.rdf" );

Convert some RSS/RDF files to Atom format:

    use XML::FeedPP ();
    my $feed = XML::FeedPP::Atom::Atom10->new();        # create empty atom file
    $feed->merge( "rss.xml" );                          # load local RSS file
    $feed->merge( "http://www.kawa.net/index.rdf" );    # load remote RDF file
    my $now = time();
    $feed->pubDate( $now );                             # touch date
    my $atom = $feed->to_string();                      # get Atom source code

=head1 DESCRIPTION

C<XML::FeedPP> is an all-purpose syndication utility that parses and
publishes RSS 2.0, RSS 1.0 (RDF), Atom 0.3 and 1.0 feeds.
It allows you to add new content, merge feeds, and convert among
these various formats.
It is a pure Perl implementation and does not require any other
module except for XML::TreePP.

=head1 METHODS FOR FEED

=head2  $feed = XML::FeedPP->new( "index.rss" );

This constructor method creates an C<XML::FeedPP> feed instance. The only
argument is the local filename.  The format of $source must be one of
the supported feed formats -- RSS, RDF or Atom -- or execution is
halted.

=head2  $feed = XML::FeedPP->new( "http://use.perl.org/index.rss" );

The URL on the remote web server is also available as the first argument.
L<LWP::UserAgent> is required to download it.

=head2  $feed = XML::FeedPP->new( '<rss version="2.0">...' );

The XML source code is also available as the first argument.

=head2  $feed = XML::FeedPP->new( $source, -type => $type );

The C<-type> argument allows you to specify type of $source
from choice of C<'file'>, C<'url'> or C<'string'>.

=head2  $feed = XML::FeedPP->new( $source, utf8_flag => 1 );

This makes utf8 flag on for all feed elements.
Perl 5.8.1 or later is required to use this.

Note that any other options for C<XML::TreePP> constructor are also
allowed like this. See more detail on L<XML::TreePP>.

=head2  $feed = XML::FeedPP::RSS->new( $source );

This constructor method creates an instance for an RSS 2.0 feed.
The first argument is optional, but must be valid an RSS source if specified.
This method returns an empty instance when $source is undefined.

=head2  $feed = XML::FeedPP::RDF->new( $source );

This constructor method creates an instance for RSS 1.0 (RDF) feed.
The first argument is optional, but must be an RDF source if specified.
This method returns an empty instance when $source is undefined.

=head2  $feed = XML::FeedPP::Atom->new( $source );

This constructor method creates an instance for an Atom 0.3/1.0 feed.
The first argument is optional, but must be an Atom source if specified.
This method returns an empty instance when $source is undefined.

Atom 1.0 feed is also supported since C<XML::FeedPP> version 0.30.
Atom 0.3 is still default, however, future version of this module
would create Atom 1.0 as default.

=head2  $feed = XML::FeedPP::Atom::Atom03->new();

This creates an empty Atom 0.3 instance obviously.

=head2  $feed = XML::FeedPP::Atom::Atom10->new();

This creates an empty Atom 1.0 instance intended.

=head2  $feed = XML::FeedPP::RSS->new( link => $link, title => $tile, ... );

This creates a RSS instance which has C<link>, C<title> elements etc.

=head2  $feed->load( $source );

This method loads an RSS/RDF/Atom file,
much like C<new()> method does.

=head2  $feed->merge( $source );

This method merges an RSS/RDF/Atom file into the existing $feed
instance. Top-level metadata from the imported feed is incorporated
only if missing from the present feed.

=head2  $string = $feed->to_string( $encoding );

This method generates XML source as string and returns it.  The output
$encoding is optional, and the default encoding is 'UTF-8'.  On Perl
5.8 and later, any encodings supported by the Encode module are
available.  On Perl 5.005 and 5.6.1, only four encodings supported by
the Jcode module are available: 'UTF-8', 'Shift_JIS', 'EUC-JP' and
'ISO-2022-JP'.  'UTF-8' is recommended for overall compatibility.

=head2  $string = $feed->to_string( indent => 4 );

This makes the output more human readable by indenting appropriately.
This does not strictly follow the XML specification but does looks nice.

Note that any other options for C<XML::TreePP> constructor are also
allowed like this. See more detail on L<XML::TreePP>.

=head2  $feed->to_file( $filename, $encoding );

This method generate an XML file.  The output $encoding is optional,
and the default is 'UTF-8'.

=head2  $item = $feed->add_item( $link );

This method creates a new item/entry and returns its instance.
A mandatory $link argument is the URL of the new item/entry.

=head2  $item = $feed->add_item( $srcitem );

This method duplicates an item/entry and adds it to $feed.
$srcitem is a C<XML::FeedPP::*::Item> class's instance
which is returned by C<get_item()> method, as described above.

=head2  $item = $feed->add_item( link => $link, title => $tile, ... );

This method creates an new item/entry
which has C<link>, C<title> elements etc.

=head2  $item = $feed->get_item( $index );

This method returns item(s) in a $feed.
A valid zero-based array $index returns the corresponding item in the feed.
An invalid $index yields undef.
If $index is undefined in array context, it returns an array of all items.
If $index is undefined in scalar context, it returns the number of items.

=head2  @items = $feed->match_item( link => qr/.../, title => qr/.../, ... );

This method finds item(s) which match all regular expressions given.
This method returns an array of all matched items in array context.
This method returns the first matched item in scalar context.

=head2  $feed->remove_item( $index or $link );

This method removes an item/entry specified by zero-based array index or
link URL.

=head2  $feed->clear_item();

This method removes all items/entries from the $feed.

=head2  $feed->sort_item();

This method sorts the order of items in $feed by C<pubDate>.

=head2  $feed->uniq_item();

Reduces the list of items, not to include duplicates.  RDF and Atoms
have a guid attribute to defined uniqueness, for RSS we use the link.

=head2  $feed->normalize();

This method calls both the C<sort_item()> and C<uniq_item()> methods.

=head2  $feed->limit_item( $num );

Removes items in excess of the specified numeric limit. Items at the
end of the list are removed. When preceded by C<sort_item()> or
C<normalize()>, this deletes more recent items.

=head2  $feed->xmlns( "xmlns:media" => "http://search.yahoo.com/mrss" );

Adds an XML namespace at the document root of the feed.

=head2  $url = $feed->xmlns( "xmlns:media" );

Returns the URL of the specified XML namespace.

=head2  @list = $feed->xmlns();

Returns the list of all XML namespaces used in $feed.

=head1  METHODS FOR CHANNEL

=head2  $feed->title( $text );

This method sets/gets the feed's C<title> element,
returning its current value when $title is undefined.

=head2  $feed->description( $html );

This method sets/gets the feed's C<description> element in plain text or HTML,
returning its current value when $html is undefined.
It is mapped to C<content> element for Atom 0.3/1.0.

=head2  $feed->pubDate( $date );

This method sets/gets the feed's C<pubDate> element for RSS,
returning its current value when $date is undefined.
It is mapped to C<dc:date> element for RDF,
C<modified> for Atom 0.3, and C<updated> for Atom 1.0.
See also L</DATE AND TIME FORMATS> section below.

=head2  $feed->copyright( $text );

This method sets/gets the feed's C<copyright> element for RSS,
returning its current value when $text is undefined.
It is mapped to C<dc:rights> element for RDF,
C<copyright> for Atom 0.3, and C<rights> for Atom 1.0.

=head2  $feed->link( $url );

This method sets/gets the URL of the web site as the feed's C<link> element,
returning its current value when the $url is undefined.

=head2  $feed->language( $lang );

This method sets/gets the feed's C<language> element for RSS,
returning its current value when the $lang is undefined.
It is mapped to C<dc:language> element for RDF,
C<feed xml:lang=""> for Atom 0.3/1.0.

=head2  $feed->image( $url, $title, $link, $description, $width, $height )

This method sets/gets the feed's C<image> element and its child nodes,
returning a list of current values when any arguments are undefined.

=head1  METHODS FOR ITEM

=head2  $item->title( $text );

This method sets/gets the item's C<title> element,
returning its current value when the $text is undefined.

=head2  $item->description( $html );

This method sets/gets the item's C<description> element in HTML or plain text,
returning its current value when $text is undefined.
It is mapped to C<content> element for Atom 0.3/1.0.

=head2  $item->pubDate( $date );

This method sets/gets the item's C<pubDate> element,
returning its current value when $date is undefined.
It is mapped to C<dc:date> element for RDF,
C<modified> for Atom 0.3, and C<updated> for Atom 1.0.
See also L</DATE AND TIME FORMATS> section below.

=head2  $item->category( $text );

This method sets/gets the item's C<category> element.
returning its current value when $text is undefined.
It is mapped to C<dc:subject> element for RDF, and ignored for Atom 0.3.

=head2  $item->author( $name );

This method sets/gets the item's C<author> element,
returning its current value when $name is undefined.
It is mapped to C<dc:creator> element for RDF,
C<author> for Atom 0.3/1.0.

=head2  $item->guid( $guid, isPermaLink => $bool );

This method sets/gets the item's C<guid> element,
returning its current value when $guid is undefined.

It is mapped to C<id> element for Atom, and ignored for RDF.  In case of
RSS, it will return a HASH.  The C<isParmaLink> is supported by RSS only,
and optional.

=head2  $item->set( $key => $value, ... );

This method sets customized node values or attributes.
See also L</ACCESSOR AND MUTATORS> section below.

=head2  $value = $item->get( $key );

This method returns the node value or attribute.
See also L</ACCESSOR AND MUTATORS> section below.

=head2  $link = $item->link();

This method returns the item's C<link> element.

=head1  ACCESSOR AND MUTATORS

This module understands only subset of C<rdf:*>, C<dc:*> modules
and RSS/RDF/Atom's default namespaces by itself.
There are NO native methods for any other external modules, such as C<media:*>.
But C<set()> and C<get()> methods are available to get/set
the value of any elements or attributes for these modules.

=head2  $item->set( "module:name" => $value );

This sets the value of the child node:

    <item><module:name>$value</module:name>...</item>

=head2  $item->set( "module:name@attr" => $value );

This sets the value of the child node's attribute:

    <item><module:name attr="$value" />...</item>

=head2  $item->set( "@attr" => $value );

This sets the value of the item's attribute:

    <item attr="$value">...</item>

=head2  $item->set( "hoge/pomu@hare" => $value );

This code sets the value of the child node's child node's attribute:

    <item><hoge><pomu attr="$value" /></hoge>...</item>

=head1  DATE AND TIME FORMATS

C<XML::FeedPP> allows you to describe date/time using any of the three
following formats:

=head2  $date = "Thu, 23 Feb 2006 14:43:43 +0900";

This is the HTTP protocol's preferred format and RSS 2.0's native
format, as defined by RFC 1123.

=head2  $date = "2006-02-23T14:43:43+09:00";

W3CDTF is the native format of RDF, as defined by ISO 8601.

=head2  $date = 1140705823;

The last format is the number of seconds since the epoch,
C<1970-01-01T00:00:00Z>.
You know, this is the native format of Perl's C<time()> function.

=head1 USING MEDIA RSS

To publish Media RSS, add the C<media> namespace then use C<set()>
setter method to manipulate C<media:content> element, etc.

    my $feed = XML::FeedPP::RSS->new();
    $feed->xmlns('xmlns:media' => 'http://search.yahoo.com/mrss/');
    my $item = $feed->add_item('http://www.example.com/index.html');
    $item->set('media:content@url' => 'http://www.example.com/image.jpg');
    $item->set('media:content@type' => 'image/jpeg');
    $item->set('media:content@width' => 640);
    $item->set('media:content@height' => 480);

=head1 MODULE DEPENDENCIES

C<XML::FeedPP> requires only L<XML::TreePP>
which likewise is a pure Perl implementation.
The standard L<LWP::UserAgent> is required
to download feeds from remote web servers.
C<Jcode.pm> is required to convert Japanese encodings on Perl 5.005
and 5.6.1, but is NOT required on Perl 5.8.x and later.

=head1 AUTHOR

Yusuke Kawasaki, http://www.kawa.net/

=head1 COPYRIGHT

The following copyright notice applies to all the files provided in
this distribution, including binary files, unless explicitly noted
otherwise.

Copyright 2006-2011 Yusuke Kawasaki

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

