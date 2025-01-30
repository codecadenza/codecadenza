/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.resource.db;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration;

/**
 * <p>
 * JAXB mapping class for database templates
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class DatabaseTemplate implements Serializable {
	private static final long serialVersionUID = -8096740924765075708L;

	@XmlAttribute(name = "vendor_group", required = true)
	private DBVendorGroupEnumeration vendorGroup;

	@XmlElement(name = "driver_name", required = true)
	private String driverName;

	@XmlElement(name = "sample_url", required = true)
	private String sampleURL;

	@XmlElement(name = "hibernate_dialect", required = true)
	private String hibernateDialect;

	@XmlElement(name = "max_identifier_length", required = true)
	private int maxIdentifierLength;

	@XmlElement(name = "eclipselink_target_dbname", required = true)
	private String eclipseLinkTargetDBName;

	@XmlElement(name = "identifier_regex", required = true)
	private String identifierRegEx;

	@XmlElement(name = "default_schema_name", required = false)
	private String defaultSchemaName;

	@XmlElement(required = false)
	private String comment;

	@XmlElement(name = "reserved_words", required = true)
	private String reservedWords;

	@XmlElement(name = "identity_column_support", required = true)
	private boolean identityColumnSupport;

	@XmlElement(name = "sequence_support", required = true)
	private boolean sequenceSupport;

	@XmlElement(name = "conversion_type", required = true)
	private IdentifierStyleEnumeration identifierStyle;

	@XmlElement(name = "column_type_mapping", required = false)
	private List<DBColumnType> mappings = new ArrayList<>();

	/**
	 * @return the vendor group
	 */
	public DBVendorGroupEnumeration getVendorGroup() {
		return this.vendorGroup;
	}

	/**
	 * @param vendorGroup
	 */
	public void setVendorGroup(DBVendorGroupEnumeration vendorGroup) {
		this.vendorGroup = vendorGroup;
	}

	/**
	 * @return the driver name
	 */
	public String getDriverName() {
		return this.driverName;
	}

	/**
	 * @param driverName
	 */
	public void setDriverName(String driverName) {
		this.driverName = driverName;
	}

	/**
	 * @return the sample URL
	 */
	public String getSampleURL() {
		return this.sampleURL;
	}

	/**
	 * @param sampleURL
	 */
	public void setSampleURL(String sampleURL) {
		this.sampleURL = sampleURL;
	}

	/**
	 * @return the Hibernate dialect
	 */
	public String getHibernateDialect() {
		return this.hibernateDialect;
	}

	/**
	 * @param hibernateDialect
	 */
	public void setHibernateDialect(String hibernateDialect) {
		this.hibernateDialect = hibernateDialect;
	}

	/**
	 * @return the max. identifier length
	 */
	public int getMaxIdentifierLength() {
		return this.maxIdentifierLength;
	}

	/**
	 * @param maxIdentifierLength
	 */
	public void setMaxIdentifierLength(int maxIdentifierLength) {
		this.maxIdentifierLength = maxIdentifierLength;
	}

	/**
	 * @return the EclipseLink target DB name
	 */
	public String getEclipseLinkTargetDBName() {
		return this.eclipseLinkTargetDBName;
	}

	/**
	 * @param eclipseLinkTargetDBName
	 */
	public void setEclipseLinkTargetDBName(String eclipseLinkTargetDBName) {
		this.eclipseLinkTargetDBName = eclipseLinkTargetDBName;
	}

	/**
	 * @return the regular expression for identifiers
	 */
	public String getIdentifierRegEx() {
		return this.identifierRegEx;
	}

	/**
	 * @param identifierRegEx
	 */
	public void setIdentifierRegEx(String identifierRegEx) {
		this.identifierRegEx = identifierRegEx;
	}

	/**
	 * @return the default schema name
	 */
	public String getDefaultSchemaName() {
		return this.defaultSchemaName;
	}

	/**
	 * @param defaultSchemaName
	 */
	public void setDefaultSchemaName(String defaultSchemaName) {
		this.defaultSchemaName = defaultSchemaName;
	}

	/**
	 * @return the comment
	 */
	public String getComment() {
		return this.comment;
	}

	/**
	 * @param comment
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

	/**
	 * @return the reserved words
	 */
	public String getReservedWords() {
		return this.reservedWords;
	}

	/**
	 * @param reservedWords
	 */
	public void setReservedWords(String reservedWords) {
		this.reservedWords = reservedWords;
	}

	/**
	 * @return true if identity columns are supported
	 */
	public boolean isIdentityColumnSupport() {
		return this.identityColumnSupport;
	}

	/**
	 * @param identityColumnSupport
	 */
	public void setIdentityColumnSupport(boolean identityColumnSupport) {
		this.identityColumnSupport = identityColumnSupport;
	}

	/**
	 * @return true if sequences are supported
	 */
	public boolean isSequenceSupport() {
		return this.sequenceSupport;
	}

	/**
	 * @param sequenceSupport
	 */
	public void setSequenceSupport(boolean sequenceSupport) {
		this.sequenceSupport = sequenceSupport;
	}

	/**
	 * @return the identifier style
	 */
	public IdentifierStyleEnumeration getIdentifierStyle() {
		return this.identifierStyle;
	}

	/**
	 * @param identifierStyle
	 */
	public void setIdentifierStyle(IdentifierStyleEnumeration identifierStyle) {
		this.identifierStyle = identifierStyle;
	}

	/**
	 * @return a list of elements
	 */
	public List<DBColumnType> getMappings() {
		return this.mappings;
	}

	/**
	 * @param mappings
	 */
	public void setMappings(List<DBColumnType> mappings) {
		this.mappings = mappings;
	}

}
