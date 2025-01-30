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

/**
 * <p>
 * JAXB mapping class for database column types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class DBColumnType implements Serializable {
	private static final long serialVersionUID = 284235995147480916L;

	@XmlAttribute(name = "name", required = true)
	private String name;

	@XmlAttribute(name = "omit_length_information", required = true)
	private boolean omitSizeInformation;

	@XmlElement(name = "java_type", required = false)
	private List<JavaType> javaTypes = new ArrayList<>();

	/**
	 * @return the database type name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return true if the omit size information flag is set
	 */
	public boolean isOmitSizeInformation() {
		return this.omitSizeInformation;
	}

	/**
	 * @param omitSizeInformation
	 */
	public void setOmitLengthInformation(boolean omitSizeInformation) {
		this.omitSizeInformation = omitSizeInformation;
	}

	/**
	 * @return a list of elements
	 */
	public List<JavaType> getJavaTypes() {
		return this.javaTypes;
	}

	/**
	 * @param javaTypes
	 */
	public void setJavaTypes(List<JavaType> javaTypes) {
		this.javaTypes = javaTypes;
	}

}
