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

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SEI;

import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.IntegrationProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Build module for integration service end-point interfaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationSEIBuildModule extends AbstractBuildModule {
	private final BuildArtifactType artifactType;
	private final IntegrationModule integrationModule;
	private final IIntegrationProjectFilesGenerator integrationProjectFilesGenerator;

	/**
	 * Constructor
	 * @param buildArtifact
	 * @param artifactType
	 */
	public IntegrationSEIBuildModule(BuildArtifact buildArtifact, BuildArtifactType artifactType) {
		super(buildArtifact);

		this.artifactType = artifactType;
		this.integrationModule = project.getIntegrationModuleByArtifact(artifactType);
		this.integrationProjectFilesGenerator = IntegrationProjectFilesGeneratorFactory.getGenerator(integrationModule, artifactType);
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

		addBeansXML(configFiles, artifactType);

		return configFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getPackages()
	 */
	@Override
	public List<String> getPackages() {
		final List<String> packages = super.getPackages();

		if (artifactType == BuildArtifactType.INTEGRATION_SEI_KAFKA) {
			packages.add(integrationModule.getNamespace().toString() + SUB_PACKAGE_INT_AVRO);

			if (integrationModule.hasClientArtifact())
				packages.add(integrationModule.getNamespace().toString() + SUB_PACKAGE_INT_SEI);
		}
		else
			packages.add(integrationModule.getNamespace().toString());

		return packages;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#createInitialSourceFiles()
	 */
	@Override
	public void createInitialSourceFiles() throws Exception {
		for (final JavaFile sourceFile : integrationProjectFilesGenerator.createSourceFiles())
			EclipseIDEService.createJavaFile(sourceFile);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getCompilerOptions()
	 */
	@Override
	public Map<String, String> getCompilerOptions() {
		final var options = super.getCompilerOptions();

		if (artifactType == BuildArtifactType.INTEGRATION_SEI_KAFKA) {
			// Change the compiler options in order to avoid warnings due to Java classes that are created by the Avro generator!
			options.put(JavaCore.COMPILER_PB_UNUSED_IMPORT, JavaCore.IGNORE);
			options.put(JavaCore.COMPILER_PB_UNUSED_WARNING_TOKEN, JavaCore.IGNORE);
			options.put(JavaCore.COMPILER_PB_DEAD_CODE, JavaCore.IGNORE);
			options.put(JavaCore.COMPILER_PB_UNUSED_LOCAL, JavaCore.IGNORE);
			options.put(JavaCore.COMPILER_PB_RAW_TYPE_REFERENCE, JavaCore.IGNORE);
		}

		return options;
	}
}
