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
package net.codecadenza.eclipse.service.build;

import java.util.List;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * <p>
 * Interface that must be implemented by all project build services
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IProjectBuildService {
	/**
	 * @return the default build configuration for a given project
	 */
	List<BuildArtifact> getDefaultBuildConfiguration();

	/**
	 * Build a project
	 * @param deployDir
	 * @param monitor
	 * @throws Exception if the project build operation has failed
	 */
	void buildProject(String deployDir, IProgressMonitor monitor) throws Exception;

	/**
	 * Rebuild the orm.xml file
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildORMXML() throws Exception;

	/**
	 * Rebuild the persistence.xml file
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildPersistenceUnit() throws Exception;

	/**
	 * Rebuild the data source file
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildDataSourceFile() throws Exception;

	/**
	 * Rebuild all security-related files
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildSecurity() throws Exception;

	/**
	 * Rebuild all objects
	 * @param monitor
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildAllObjects(IProgressMonitor monitor) throws Exception;

	/**
	 * Rebuild the service for saved queries
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildSavedQueryService() throws Exception;

	/**
	 * Rebuild the logging service
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildLoggingService() throws Exception;

	/**
	 * Build all objects of the given namespace
	 * @param namespace
	 * @param rebuildPersitenceXML
	 * @param monitor
	 * @throws Exception if the build operation has failed
	 */
	void buildObjectsOfNamespace(Namespace namespace, boolean rebuildPersitenceXML, IProgressMonitor monitor) throws Exception;

	/**
	 * Rebuild the configuration files of a Spring Boot application
	 * @throws Exception if the rebuild operation has failed
	 */
	void rebuildSpringBootConfigurationFiles() throws Exception;

}
