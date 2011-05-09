# ----------------------------------------------------------------
    use strict;
    use Test::More tests => 4;
    BEGIN { use_ok('XML::FeedPP') };
# ----------------------------------------------------------------
    package MyFeedPP::MyRDF;
    use base 'XML::FeedPP::RDF';

    sub item_class {
        'MyFeedPP::MyRDF::MyItem';
    }

    package MyFeedPP::MyRDF::MyItem;
    use base 'XML::FeedPP::RDF::Item';

    sub description {
        my $self = shift;
        my $desc = shift;
        if (! defined $desc) {
            my $content = $self->get_value('content:encoded');
            $desc = $$desc if ref $desc;
            return $content if defined $content;
            return $self->SUPER::description();
        } else {
            my $ref = ref $desc ? $desc : \$desc;
            $self->set_value('content:encoded' => $ref, @_);
            $self->SUPER::description($desc, @_);
        }
    }
# ----------------------------------------------------------------
package main;

# http://web.resource.org/rss/1.0/modules/content/
my $SOURCE = <<EOT;
<?xml version="1.0" encoding="utf-8"?> 

<rdf:RDF 
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
  xmlns:content="http://purl.org/rss/1.0/modules/content/"
  xmlns="http://purl.org/rss/1.0/"
> 

  <channel rdf:about="http://example.org/rss.rdf">
    <title>Example Feed</title>
    <link>http://www.example.org</link>
    <description>Simply for the purpose of demonstration.</description>

    <items>
      <rdf:Seq>
        <rdf:li resource="http://example.org/item/" />
      </rdf:Seq>
    </items>
    
  </channel>

  <item rdf:about="http://example.org/item/">
    <title>The Example Item</title> 
    <link>http://example.org/item/</link>
    <content:encoded><![CDATA[<p>What a <em>beautiful</em> day!</p>]]></content:encoded>
  </item> 
</rdf:RDF>
EOT

my $DESCB = '<p>What a <em>beautiful</em> day!</p>';
my $DESCW = '<p>What a <em>wonderful</em> day!</p>';
my $ELEMW = '<content:encoded><![CDATA[<p>What a <em>wonderful</em> day!</p>]]></content:encoded>';

# ----------------------------------------------------------------
{
    my $feed1 = MyFeedPP::MyRDF->new($SOURCE);
    my $item1 = $feed1->get_item(0);
    my $desc1 = $item1->description;
    is($desc1, $DESCB, 'get content:encoded');

    $item1->description($DESCW);
    my $desc2 = $item1->description;
    is($desc2, $DESCW, 'set and get content:encoded');

    my $out = $feed1->to_string;
    like($out, qr/\Q$ELEMW\E/, 'to_string');
}
# ----------------------------------------------------------------
;1;
# ----------------------------------------------------------------
