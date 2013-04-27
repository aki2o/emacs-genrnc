<?xml version="1.0" encoding="UTF-8"?>
<!--
Copyright or © or Copr. Nicolas Debeissat

nicolas.debeissat@gmail.com (http://debeissat.nicolas.free.fr/)

This software is a computer program whose purpose is to convert a
XSD schema into a RelaxNG schema.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the CeCILL
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" exclude-result-prefixes="xs" version="1.0">
	
	<xsl:output indent="yes" method="xml"/>
	
	<xsl:preserve-space elements="*"/>
	
	<xsl:template match="/xs:schema">
		<rng:grammar>
			<xsl:for-each select="namespace::*">
				<xsl:if test="local-name() != 'xs'">
					<xsl:copy/>
				</xsl:if>
			</xsl:for-each>
			<xsl:attribute name="ns"><xsl:value-of select="@targetNamespace"/></xsl:attribute>
			<xsl:attribute name="datatypeLibrary">http://www.w3.org/2001/XMLSchema-datatypes</xsl:attribute>

			<!-- This is an effort to put out right schema if not exist definition referenced by current element -->
			<xsl:for-each select="//xs:*[@type]">
				<xsl:call-template name="defNotExistReference"><xsl:with-param name="ref" select="@type"/></xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="//xs:*[@ref]">
				<xsl:call-template name="defNotExistReference"><xsl:with-param name="ref" select="@ref"/></xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="//xs:*[@base]">
				<xsl:call-template name="defNotExistReference"><xsl:with-param name="ref" select="@base"/></xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="//xs:*[@memberTypes]">
				<xsl:call-template name="defNotExistReference"><xsl:with-param name="ref" select="@memberTypes"/></xsl:call-template>
			</xsl:for-each>

			<xsl:apply-templates/>
		</rng:grammar>
	</xsl:template>
	
	<!-- in order to manage occurencies (and defaut) attributes goes there
		 before going to mode="content" templates -->
	<xsl:template match="xs:*">
		<xsl:call-template name="occurencies"/>
	</xsl:template>
	
	<xsl:template match="comment()">
		<xsl:copy/>
	</xsl:template>
	
	<xsl:template match="xs:annotation">
		<a:documentation>
			<xsl:apply-templates/>
		</a:documentation>
	</xsl:template>
	
	<xsl:template match="xs:documentation">
		<xsl:copy-of select="child::node()"/>
	</xsl:template>
	
	<xsl:template match="xs:appinfo">
		<xsl:copy-of select="child::node()"/>
	</xsl:template>
	
	<xsl:template match="xs:union">
		<rng:choice>
			<xsl:apply-templates select="@memberTypes"/>
			<xsl:apply-templates/>
		</rng:choice>
	</xsl:template>
	
	<xsl:template match="@memberTypes">
		<xsl:call-template name="declareMemberTypes">
			<xsl:with-param name="memberTypes" select="."/>
		</xsl:call-template>
	</xsl:template>
	
	<xsl:template match="xs:list">
		<rng:list>
			<xsl:apply-templates select="@itemType"/>
			<xsl:apply-templates/>
		</rng:list>
	</xsl:template>
	
	<xsl:template match="@itemType">
		<xsl:call-template name="type">
			<xsl:with-param name="type" select="."/>
		</xsl:call-template>
	</xsl:template>
	
	<xsl:template match="xs:complexType[@name]|xs:simpleType[@name]|xs:group[@name]|xs:attributeGroup[@name]">
		<!-- the schemas may be included several times, so it needs a combine attribute
                                     (the attributes are inversed :-) at the transformation) -->
		<xsl:variable name="currns" select="/xs:schema/xs:genrnc[@name='']"/>
		<xsl:choose>
			<xsl:when test="$currns!=''">
				<rng:define name="{concat($currns, '.', @name)}" combine="interleave">
					<xsl:apply-templates/>
				</rng:define>
			</xsl:when>
			<xsl:otherwise>
				<rng:define name="{@name}" combine="interleave">
					<xsl:apply-templates/>
				</rng:define>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- when finds a ref attribute replace it by its type call (ref name="" or type) -->	
	<xsl:template match="xs:*[@ref]" mode="content">
		<xsl:call-template name="type">
			<xsl:with-param name="type" select="@ref"/>
			<xsl:with-param name="ref" select="true()"/>
		</xsl:call-template>
	</xsl:template>
	
    <!-- the <xs:simpleType> and <xs:complexType without name attribute are ignored -->
	<xsl:template match="xs:sequence|xs:complexContent|xs:simpleType|xs:complexType">
		<xsl:apply-templates/>
		<xsl:choose>
			<xsl:when test="xs:any|xs:choice|xs:element|xs:group|xs:sequence|xs:restriction|xs:extension|xs:list|xs:union|xs:simpleContent|xs:complexContent|xs:attribute|xs:attributeGroup|xs:anyAttribute">
			</xsl:when>
			<xsl:when test="parent::xs:element">
				<!-- If this is final element and parent is element element, insert empty -->
				<rng:empty/>
			</xsl:when>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="xs:extension[@base]">
		<xsl:call-template name="type">
			<xsl:with-param name="type" select="@base"/>
		</xsl:call-template>
	</xsl:template>
	
	<!-- This is wrong syntax, but make an effort to put out right schema -->
	<xsl:template match="xs:extension[not(@base)]">
		<xsl:apply-templates/>
	</xsl:template>
    
	<xsl:template match="xs:element[@name]">
		<xsl:choose>
			<xsl:when test="parent::xs:schema">
				<xsl:call-template name="rootelement">
					<xsl:with-param name="currnm" select="@name"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="occurencies"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="xs:restriction[@base]">
		<xsl:choose>
			<xsl:when test="xs:enumeration[@value]">
				<rng:choice>
					<xsl:apply-templates/>
				</rng:choice>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="type">
					<xsl:with-param name="type" select="@base"/>
					<xsl:with-param name="org" select="name()"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- This is wrong syntax, but make an effort to put out right schema -->
	<xsl:template match="xs:restriction[not(@base)]">
		<xsl:choose>
			<xsl:when test="xs:enumeration[@value]">
				<rng:choice>
					<xsl:apply-templates/>
				</rng:choice>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="xs:enumeration[@value]">
		<rng:value>
			<xsl:value-of select="@value"/>
		</rng:value>
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="xs:minInclusive[@value]|xs:minExclusive[@value]|xs:maxInclusive[@value]|xs:maxExclusive[@value]|xs:pattern[@value]">
		<rng:param name="{local-name()}">
			<xsl:value-of select="@value"/>
		</rng:param>
	</xsl:template>
	
	<xsl:template match="xs:all">
		<rng:interleave>
			<xsl:for-each select="child::text()[normalize-space(.) != ''] | child::*">
				<rng:optional>
					<xsl:apply-templates select="current()"/>
				</rng:optional>
			</xsl:for-each>
		</rng:interleave>
	</xsl:template>
	
	<xsl:template match="xs:import|xs:include|xs:redefine">
		<xsl:if test="@schemaLocation">
			<rng:include>
				<xsl:attribute name="href"><xsl:value-of select="concat(substring-before(@schemaLocation, '.xsd'),'.rng')"/></xsl:attribute>
				<xsl:if test="@namespace">
					<xsl:attribute name="ns"><xsl:value-of select="@namespace"/></xsl:attribute>
				</xsl:if>
			</rng:include>
		</xsl:if>
		<xsl:apply-templates/>
	</xsl:template>
    
	<xsl:template match="@default">
		<a:documentation>
            default value is : <xsl:value-of select="."/>
		</a:documentation>
	</xsl:template>
    
	<xsl:template match="xs:attribute[@name]" mode="content">
		<rng:attribute name="{@name}">
			<xsl:apply-templates select="@default" mode="attributeDefaultValue"/>
			<!-- there can be no type attribute to <xs:attribute>, in this case, the type is defined in 
				 a <xs:simpleType> or a <xs:complexType> inside -->
			<xsl:choose>
				<xsl:when test="@type">
					<xsl:call-template name="type">
						<xsl:with-param name="type" select="@type"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates/>
				</xsl:otherwise>
			</xsl:choose>
		</rng:attribute>
	</xsl:template>
	
	<xsl:template match="@default" mode="attributeDefaultValue">
    	<xsl:attribute name="defaultValue" namespace="http://relaxng.org/ns/compatibility/annotations/1.0">
    		<xsl:value-of select="."/>
    	</xsl:attribute>
	</xsl:template>
	
	<xsl:template match="xs:any" mode="content">
		<rng:element>
			<rng:anyName/>
			<rng:text/>
		</rng:element>
	</xsl:template>
	
	<xsl:template match="xs:anyAttribute" mode="content">
		<rng:attribute>
			<rng:anyName/>
			<rng:text/>
		</rng:attribute>
	</xsl:template>
	
	<xsl:template match="xs:choice" mode="content">
		<rng:choice>
			<xsl:choose>
				<xsl:when test="xs:any|xs:choice|xs:element|xs:group|xs:sequence">
					<xsl:apply-templates/>
				</xsl:when>
				<xsl:otherwise>
					<rng:empty/>
				</xsl:otherwise>
			</xsl:choose>
		</rng:choice>
	</xsl:template>
	
	<xsl:template match="xs:element" mode="content">
		<rng:element name="{@name}">
			<xsl:choose>
				<xsl:when test="@type">
					<xsl:call-template name="type">
						<xsl:with-param name="type" select="@type"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="xs:simpleType|xs:complexType|xs:key|xs:keyref|xs:unique">
					<xsl:apply-templates/>
				</xsl:when>
				<xsl:otherwise>
					<rng:empty/>
					<xsl:apply-templates/>
				</xsl:otherwise>
			</xsl:choose>
		</rng:element>
	</xsl:template>
	
	<xsl:template match="xs:genrnc">
	</xsl:template>

	<xsl:template name="occurencies">
		<xsl:apply-templates select="@default"/>
		<xsl:choose>
			<xsl:when test="@use and @use='optional'">
				<rng:optional>
					<xsl:apply-templates select="current()" mode="content"/>
				</rng:optional>
			</xsl:when>
			<xsl:when test="@maxOccurs and @maxOccurs='unbounded'">
				<xsl:choose>
					<xsl:when test="@minOccurs and @minOccurs='0'">
						<rng:zeroOrMore>
							<xsl:apply-templates select="current()" mode="content"/>
						</rng:zeroOrMore>
					</xsl:when>
					<xsl:otherwise>
						<rng:oneOrMore>
							<xsl:apply-templates select="current()" mode="content"/>
						</rng:oneOrMore>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="@minOccurs and @minOccurs='0'">
				<rng:optional>
					<xsl:apply-templates select="current()" mode="content"/>
				</rng:optional>
			</xsl:when>
			<!-- here minOccurs is present but not = 0 -->
			<xsl:when test="@minOccurs">
				<xsl:call-template name="loopUntilZero">
					<xsl:with-param name="nbLoops" select="@minOccurs"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="current()" mode="content"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="loopUntilZero">
		<xsl:param name="nbLoops"/>
		<xsl:if test="$nbLoops > 0">
			<xsl:apply-templates select="current()" mode="content"/>
			<xsl:call-template name="loopUntilZero">
				<xsl:with-param name="nbLoops" select="$nbLoops - 1"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>

	<xsl:template name="type">
		<xsl:param name="type"/>
		<xsl:param name="org"/>
		<xsl:param name="ref"/>
		<xsl:variable name="prefix" select="substring-before($type, ':')"/>
		<xsl:variable name="currns" select="/xs:schema/xs:genrnc[@name=$prefix]"/>
		<xsl:variable name="ltype" select="substring-after($type, ':')"/>
		<xsl:choose>
			<xsl:when test="$prefix!=''">
				<xsl:choose>
					<xsl:when test="$ltype='anyType'">
						<xsl:call-template name="data">
							<xsl:with-param name="type" select="'string'"/>
						</xsl:call-template>
					</xsl:when>
					<xsl:when test="contains($org, 'restriction')">
						<xsl:call-template name="data">
							<xsl:with-param name="type" select="$ltype"/>
						</xsl:call-template>
					</xsl:when>
					<xsl:when test="$currns!=''">
						<rng:ref name="{concat($currns, '.', $ltype)}"/>
						<xsl:apply-templates/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="data">
							<xsl:with-param name="type" select="$ltype"/>
						</xsl:call-template>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:otherwise>
				<xsl:choose>
					<xsl:when test="$type='anyType'">
						<xsl:call-template name="data">
							<xsl:with-param name="type" select="'string'"/>
						</xsl:call-template>
					</xsl:when>
					<xsl:when test="contains($org, 'restriction')">
						<xsl:call-template name="data">
							<xsl:with-param name="type" select="$type"/>
						</xsl:call-template>
					</xsl:when>
					<xsl:when test="$currns!=''">
						<rng:ref name="{concat($currns, '.', $type)}"/>
						<xsl:apply-templates/>
					</xsl:when>
					<xsl:when test="$ref">
						<rng:ref name="{$type}"/>
						<xsl:apply-templates/>
					</xsl:when>
					<xsl:when test="$type='string' or $type='boolean' or $type='float' or $type='double' or $type='decimal' or $type='dateTime' or $type='duration' or $type='hexBinary' or $type='base64Binary' or $type='anyURI' or $type='ID' or $type='IDREF' or $type='ENTITY' or $type='NOTATION' or $type='normalizedString' or $type='token' or $type='language' or $type='IDREFS' or $type='ENTITIES' or $type='NMTOKEN' or $type='NMTOKENS' or $type='Name' or $type='QName' or $type='NCName' or $type='integer' or $type='nonNegativeInteger' or $type='positiveInteger' or $type='nonPositiveInteger' or $type='negativeInteger' or $type='byte' or $type='int' or $type='long' or $type='short' or $type='unsignedByte' or $type='unsignedInt' or $type='unsignedLong' or $type='unsignedShort' or $type='date' or $type='time' or $type='gYearMonth' or $type='gYear' or $type='gMonthDay' or $type='gDay' or $type='gMonth'">
						<xsl:call-template name="data">
							<xsl:with-param name="type" select="$type"/>
						</xsl:call-template>
					</xsl:when>
					<xsl:otherwise>
						<rng:ref name="{$type}"/>
						<xsl:apply-templates/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="data">
		<xsl:param name="type"/>
		<rng:data type="{$type}">
			<xsl:for-each select="xs:minInclusive[@value]|xs:minExclusive[@value]|xs:maxInclusive[@value]|xs:maxExclusive[@value]|xs:pattern[@value]">
				<rng:param name="{local-name()}"><xsl:value-of select="@value"/></rng:param>
			</xsl:for-each>
		</rng:data>
		<xsl:apply-templates select="xs:group|xs:all|xs:choice|xs:sequence|xs:attribute|xs:attributeGroup|xs:anyAttribute|xs:annotation|xs:fractionDigits|xs:enumeration|xs:length|xs:maxLength|xs:minLength|xs:simpleType|xs:totalDigits|xs:whiteSpace"/>
	</xsl:template>
    
	<xsl:template name="declareMemberTypes">
		<xsl:param name="memberTypes"/>
		<xsl:choose>
            <xsl:when test="contains($memberTypes, ' ')">
				<xsl:call-template name="type">
					<xsl:with-param name="type" select="substring-before($memberTypes, ' ')"/>
				</xsl:call-template>
                <xsl:call-template name="declareMemberTypes">
                    <xsl:with-param name="memberTypes" select="substring-after($memberTypes, ' ')"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
				<xsl:call-template name="type">
					<xsl:with-param name="type" select="$memberTypes"/>
				</xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
	</xsl:template>
    
	<xsl:template name="rootelement">
		<!-- This is global element -->
		<xsl:param name="currnm"/>
		<!-- Check exist same ident of define -->
		<xsl:choose>
			<xsl:when test="//xs:complexType[@name=$currnm]|//xs:simpleType[@name=$currnm]|//xs:group[@name=$currnm]|//xs:attributeGroup[@name=$currnm]">
				<!-- If exist, ascribe its contents equal this element -->
				<rng:start combine="choice">
					<xsl:call-template name="doelement">
						<xsl:with-param name="currnm" select="$currnm"/>
					</xsl:call-template>
				</rng:start>
			</xsl:when>
			<xsl:otherwise>
				<!-- If not exist, add definition this element name -->
				<xsl:call-template name="defelement">
					<xsl:with-param name="currnm" select="$currnm"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="defelement">
		<xsl:param name="currnm"/>
		<xsl:variable name="currns" select="/xs:schema/xs:genrnc[@name='']"/>
		<xsl:choose>
			<xsl:when test="$currns!=''">
				<rng:start combine="choice">
					<rng:ref name="{concat($currns, '.', $currnm)}"/>
				</rng:start>
				<rng:define name="{concat($currns, '.', $currnm)}" combine="interleave">
					<xsl:call-template name="doelement">
						<xsl:with-param name="currnm" select="$currnm"/>
					</xsl:call-template>
				</rng:define>
			</xsl:when>
			<xsl:otherwise>
				<rng:start combine="choice">
					<rng:ref name="{$currnm}"/>
				</rng:start>
				<rng:define name="{$currnm}" combine="interleave">
					<xsl:call-template name="doelement">
						<xsl:with-param name="currnm" select="$currnm"/>
					</xsl:call-template>
				</rng:define>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="doelement">
		<xsl:param name="currnm"/>
		<xsl:choose>
			<!-- case of abstract element -->
			<xsl:when test="@abstract='true'">
				<xsl:call-template name="abselement">
					<xsl:with-param name="currnm" select="$currnm"/>
				</xsl:call-template>
			</xsl:when>
			<!-- case of normal element -->
			<xsl:otherwise>
				<xsl:apply-templates select="current()" mode="content"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="abselement">
		<!-- This is abstract element. Abstract element is defined at root. -->
		<xsl:param name="currnm"/>
		<xsl:choose>
			<!-- case of not empty -->
			<xsl:when test="/xs:schema/xs:element[@name and ( @substitutionGroup=$currnm or substring-after(@substitutionGroup, ':')=$currnm )]">
				<xsl:variable name="currns" select="/xs:schema/xs:genrnc[@name='']"/>
				<rng:choice>
					<xsl:for-each select="/xs:schema/xs:element[@name and ( @substitutionGroup=$currnm or substring-after(@substitutionGroup, ':')=$currnm )]">
						<rng:ref name="{concat($currns, '.', @name)}"/>
					</xsl:for-each>
				</rng:choice>
			</xsl:when>
			<!-- case of empty -->
			<xsl:otherwise>
				<rng:empty/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="defNotExistReference">
		<xsl:param name="ref"/>
		<xsl:choose>
            <xsl:when test="contains($ref, ' ')">
				<xsl:call-template name="defNotExistReferenceSentinel">
					<xsl:with-param name="ref" select="substring-before($ref, ' ')"/>
				</xsl:call-template>
                <xsl:call-template name="defNotExistReference">
                    <xsl:with-param name="ref" select="substring-after($ref, ' ')"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
				<xsl:call-template name="defNotExistReferenceSentinel">
					<xsl:with-param name="ref" select="$ref"/>
				</xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
	</xsl:template>

	<xsl:template name="defNotExistReferenceSentinel">
		<xsl:param name="ref"/>
		<xsl:if test="$ref!=''">
			<xsl:variable name="currns" select="/xs:schema/xs:genrnc[@name='']"/>
			<xsl:choose>
				<xsl:when test="contains($ref, ':')">
					<!-- Do init definition of reference to different namespace -->
					<xsl:variable name="prefix" select="substring-before($ref, ':')"/>
					<xsl:variable name="lname" select="substring-after($ref, ':')"/>
					<xsl:variable name="ns" select="/xs:schema/xs:genrnc[@name=$prefix]"/>
					<xsl:if test="$ns!='' and $ns!=$currns">
						<rng:define name="{concat($ns, '.', $lname)}" combine="interleave"><rng:empty/></rng:define>
					</xsl:if>
				</xsl:when>
				<xsl:when test="xs:complexType[@name=$ref]|xs:simpleType[@name=$ref]|xs:group[@name=$ref]|xs:attributeGroup[@name=$ref]|xs:element[@name=$ref]">
					<!-- Do nothing because doing definition other template -->
				</xsl:when>
				<xsl:otherwise>
					<!-- Do init definition as an effort to put out right schema if wrong syntax -->
					<xsl:choose>
						<xsl:when test="$currns!=''">
							<rng:define name="{concat($currns, '.', $ref)}" combine="interleave"><rng:empty/></rng:define>
						</xsl:when>
						<xsl:otherwise>
							<rng:define name="{$ref}" combine="interleave"><rng:empty/></rng:define>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:if>
	</xsl:template>

</xsl:stylesheet>
