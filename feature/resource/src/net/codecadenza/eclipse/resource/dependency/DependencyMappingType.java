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
package net.codecadenza.eclipse.resource.dependency;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * JAXB mapping class for dependency objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class DependencyMappingType implements Serializable {
	private static final long serialVersionUID = 1L;

	@XmlAttribute(required = true)
	private String name;

	@XmlAttribute(required = true)
	private String groupName;

	@XmlAttribute(required = false)
	private String version;

	@XmlAttribute(required = true)
	private String scope;

	@XmlAttribute(required = false)
	private String repositoryId;

	@XmlAttribute(required = false)
	private String repositoryURL;

	@XmlAttribute(required = false)
	private String type;

	@XmlElement(name = "exclusion")
	private List<DependencyMappingType> exclusions = new ArrayList<>();

	/**
	 * @return the name
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
	 * @return the group name
	 */
	public String getGroupName() {
		return this.groupName;
	}

	/**
	 * @param groupName
	 */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	/**
	 * @return the version
	 */
	public String getVersion() {
		return this.version;
	}

	/**
	 * @param version
	 */
	public void setVersion(String version) {
		this.version = version;
	}

	/**
	 * @return the scope
	 */
	public String getScope() {
		return this.scope;
	}

	/**
	 * @param scope
	 */
	public void setScope(String scope) {
		this.scope = scope;
	}

	/**
	 * @return the repository ID or null if the default repository should be used
	 */
	public String getRepositoryId() {
		return repositoryId;
	}

	/**
	 * @param repositoryId
	 */
	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}

	/**
	 * @return the repository URL or null if the default repository should be used
	 */
	public String getRepositoryURL() {
		return repositoryURL;
	}

	/**
	 * @param repositoryURL
	 */
	public void setRepositoryURL(String repositoryURL) {
		this.repositoryURL = repositoryURL;
	}

	/**
	 * @return the type (e.g. 'jar', 'pom'...) or null for the default type 'jar'
	 */
	public String getType() {
		return type;
	}

	/**
	 * @param type
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @return a list containing all dependencies that should be excluded
	 */
	public List<DependencyMappingType> getExclusions() {
		return exclusions;
	}

	/**
	 * @param exclusions
	 */
	public void setExclusions(List<DependencyMappingType> exclusions) {
		this.exclusions = exclusions;
	}

}
