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
package net.codecadenza.eclipse.service.java;

import static net.codecadenza.eclipse.shared.Constants.PACKAGE_INFO_FILE;

import net.codecadenza.eclipse.generator.java.PackageInfoGenerator;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service for package-info files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PackageInfoService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public PackageInfoService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild all package-info files for a given project. Existing files will be deleted if no XML namespace prefix is defined!
	 * @throws Exception if the operation has failed
	 */
	public void rebuildPackageInfoFiles() throws Exception {
		boolean removeFiles = false;

		if (project.getXmlNamespacePrefix() == null || project.getXmlNamespacePrefix().isEmpty())
			removeFiles = true;

		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			if (removeFiles)
				removePackageInfoFile(ns, BuildArtifactType.DTO);
			else
				rebuildPackageInfoFile(ns, BuildArtifactType.DTO);

		for (final IntegrationModule module : project.getIntegrationModules()) {
			if (project.isJavaSEApplication())
				continue;

			if (module.getTechnology() == IntegrationTechnology.SOAP)
				if (removeFiles)
					removePackageInfoFile(module.getNamespace(), BuildArtifactType.INTEGRATION_SEI_SOAP);
				else
					rebuildPackageInfoFile(module.getNamespace(), BuildArtifactType.INTEGRATION_SEI_SOAP);
		}
	}

	/**
	 * Create or update a package-info file for the given namespace
	 * @param ns
	 * @param artifactType
	 * @throws Exception if the file operation has failed
	 */
	public void rebuildPackageInfoFile(Namespace ns, BuildArtifactType artifactType) throws Exception {
		final String packageName = ns.toString();
		final String path = getPackageInfoFilePath(ns);
		final String content = PackageInfoGenerator.createPackagInfoFile(project, packageName, artifactType);

		final var packageInfoFile = new WorkspaceFile(project, artifactType, path, content);
		packageInfoFile.setContent(content);

		EclipseIDEService.createOrUpdateFile(packageInfoFile);
	}

	/**
	 * Remove a package-info file from the workspace
	 * @param ns
	 * @param artifactType
	 * @throws Exception if the remove operation has failed
	 */
	public void removePackageInfoFile(Namespace ns, BuildArtifactType artifactType) throws Exception {
		final String path = getPackageInfoFilePath(ns);
		final String projectName = project.getTargetProjectName(artifactType);

		EclipseIDEService.deleteWorkspaceFile(projectName, path);
	}

	/**
	 * @param ns
	 * @return the path of a package-info file for a given namespace and artifact type
	 */
	private String getPackageInfoFilePath(Namespace ns) {
		final String packageName = ns.toString();

		return project.getSourceFolder() + "/" + packageName.replace('.', '/') + "/" + PACKAGE_INFO_FILE;
	}

}
