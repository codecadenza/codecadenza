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
package net.codecadenza.eclipse.service.build.imp.dependency;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Class for dependency objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Dependency implements Serializable {
	private static final long serialVersionUID = 1L;

	private String name;
	private String groupName;
	private String version;
	private String scope;
	private String repositoryId;
	private String repositoryURL;
	private String type;
	private List<Dependency> exclusions = new ArrayList<>();

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
	public List<Dependency> getExclusions() {
		return exclusions;
	}

	/**
	 * @param exclusions
	 */
	public void setExclusions(List<Dependency> exclusions) {
		this.exclusions = exclusions;
	}

	/**
	 * @param other
	 * @return true if another dependency is the same
	 */
	public boolean isSameDependency(Dependency other) {
		if (groupName == null) {
			if (other.groupName != null)
				return false;
		}
		else if (!groupName.equals(other.groupName))
			return false;

		if (name == null) {
			if (other.name != null)
				return false;
		}
		else if (!name.equals(other.name))
			return false;

		if (version == null) {
			if (other.version != null)
				return false;
		}
		else if (!version.equals(other.version))
			return false;

		return true;
	}

	/**
	 * @param other
	 * @return true if another dependency is the same and the scopes of both are the same
	 */
	public boolean isSameScope(Dependency other) {
		if (!isSameDependency(other))
			return false;

		if (scope == null) {
			if (other.scope != null)
				return false;
		}
		else if (!scope.equals(other.scope))
			return false;

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((groupName == null) ? 0 : groupName.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((scope == null) ? 0 : scope.hashCode());
		result = prime * result + ((version == null) ? 0 : version.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null)
			return false;

		if (getClass() != obj.getClass())
			return false;

		final var other = (Dependency) obj;

		if (groupName == null) {
			if (other.groupName != null)
				return false;
		}
		else if (!groupName.equals(other.groupName))
			return false;

		if (name == null) {
			if (other.name != null)
				return false;
		}
		else if (!name.equals(other.name))
			return false;

		if (scope == null) {
			if (other.scope != null)
				return false;
		}
		else if (!scope.equals(other.scope))
			return false;

		if (version == null) {
			if (other.version != null)
				return false;
		}
		else if (!version.equals(other.version))
			return false;

		return true;
	}

}
