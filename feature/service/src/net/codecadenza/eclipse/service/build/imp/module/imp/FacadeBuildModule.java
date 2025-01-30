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
package net.codecadenza.eclipse.service.build.imp.module.imp;

import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Build module for facade artifacts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FacadeBuildModule extends AbstractBuildModule {
	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public FacadeBuildModule(BuildArtifact buildArtifact) {
		super(buildArtifact);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getFolders()
	 */
	@Override
	public List<String> getFolders() {
		final List<String> folderNames = super.getFolders();

		if (addBeansXML(buildArtifact.getType()))
			folderNames.add(project.getMetaInfFolder());

		return folderNames;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> getConfigurationFiles() {
		final List<WorkspaceFile> configFiles = super.getConfigurationFiles();

		addBeansXML(configFiles, BuildArtifactType.FACADE);

		return configFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getPackages()
	 */
	@Override
	public List<String> getPackages() {
		final List<String> packages = super.getPackages();
		packages.add(project.getBoundaryNamespace().toString());

		project.getBoundaryNamespace().getChildNamespaces()
				.forEach(ns -> packages.add(project.getBoundaryNamespace().toString() + "." + ns.getName()));

		return packages;
	}

}
