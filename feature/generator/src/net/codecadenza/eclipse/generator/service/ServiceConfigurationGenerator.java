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
package net.codecadenza.eclipse.generator.service;

import java.util.List;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for service configuration files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServiceConfigurationGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public ServiceConfigurationGenerator(Project project) {
		this.project = project;
	}

	/**
	 * Create or update the service configuration file. The file won't be created if the project configuration doesn't require it.
	 * @throws Exception if the generation of this file has failed
	 */
	public void createFile() throws Exception {
		// The file must be generated if a ServiceLocator is responsible for providing an instance of the given interface!
		if (!project.isBoundaryMode() || !project.isJavaSEApplication())
			return;

		// The file is not necessary for GUI technologies that are able to inject a service!
		if (project.hasJSFOrVaadinClient())
			return;

		final var path = project.getConfigFolder(BuildArtifactType.GUI) + "/service.properties";
		final var dataFile = new WorkspaceFile(project, BuildArtifactType.GUI, path, createContent());

		EclipseIDEService.createOrUpdateFile(dataFile);
	}

	/**
	 * Create the content of the file
	 * @return the generated content
	 */
	private String createContent() {
		final var b = new StringBuilder();
		final List<BoundaryBean> boundaries = project.getAllBoundariesOfProject().stream()
				.sorted((b1, b2) -> b1.getName().compareTo(b2.getName())).toList();

		// Create a line for every single boundary service by using the common Java properties file structure
		boundaries.forEach(boundary -> {
			// The fully qualified name of the service interface must be used as the property key!
			b.append(boundary.getNamespace().toString() + "." + boundary.getInterfaceName() + "=");
			b.append(boundary.getNamespace().toString() + Constants.SUB_PACKAGE_BEAN + "." + boundary.getName());
			b.append("\n");
		});

		return b.toString();
	}

}
