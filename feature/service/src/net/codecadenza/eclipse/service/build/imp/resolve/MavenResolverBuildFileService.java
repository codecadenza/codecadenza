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
package net.codecadenza.eclipse.service.build.imp.resolve;

import java.io.StringWriter;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.service.build.imp.base.AbstractMavenBuildFileService;
import net.codecadenza.eclipse.shared.Constants;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.codehaus.plexus.util.xml.Xpp3Dom;

/**
 * <p>
 * Service for creating a Maven build file that is used to resolve the dependencies of the given artifact
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MavenResolverBuildFileService extends AbstractMavenBuildFileService {
	private static final String GOAL_COPY = "copy-dependencies";
	private static final String DEPENDENCY_PLUGIN_ID = "maven-dependency-plugin";
	private static final String DEPENDENCY_ELEMENT_OUTPUT_DIR = "outputDirectory";
	private static final String DEPENDENCY_ELEMENT_EXCLUDE_TYPES = "excludeTypes";

	/**
	 * Constructor
	 * @param artifact
	 */
	public MavenResolverBuildFileService(BuildArtifact artifact) {
		super(artifact);
	}

	/**
	 * @return the build file or null if no dependencies should be resolved
	 * @throws Exception if the build file could not be created
	 */
	public WorkspaceFile createBuildFile() throws Exception {
		// The build file for resolving the dependencies is only necessary for Eclipse RCP/RAP applications
		if (artifact.getType() != BuildArtifactType.GUI || !project.hasEclipseClient())
			return null;

		mavenModel.setVersion(DEFAULT_VERSION);
		mavenModel.setPackaging(PACKAGING_TYPE_JAR);
		mavenModel.setGroupId(project.getRootNamespace().toString());
		mavenModel.setArtifactId(artifact.getName());

		final List<Dependency> dependencies = getDependencies();

		// Remove all dependencies with scope 'test'
		mavenModel.setDependencies(dependencies.stream().filter(d -> !d.getScope().equals(SCOPE_TEST)).toList());

		addDependencyPlugin();

		final var sw = new StringWriter();

		// Generate the content of the pom.xml file
		final var writer = new MavenXpp3Writer();
		writer.write(sw, mavenModel);

		return new WorkspaceFile(project, artifact.getType(), Constants.POM_XML, sw.toString());
	}

	/**
	 * Add the plug-in that is responsible for copying the dependencies to the library folder
	 */
	private void addDependencyPlugin() {
		final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);
		addElement(configuration, DEPENDENCY_ELEMENT_OUTPUT_DIR, Constants.LIB_FOLDER);
		addElement(configuration, DEPENDENCY_ELEMENT_EXCLUDE_TYPES, PACKAGING_TYPE_POM);

		final var execution = new PluginExecution();
		execution.setPhase(PHASE_GENERATE_SOURCES);
		execution.getGoals().add(GOAL_COPY);
		execution.setConfiguration(configuration);

		final var dependencyPlugin = new Plugin();
		dependencyPlugin.setArtifactId(DEPENDENCY_PLUGIN_ID);
		dependencyPlugin.addExecution(execution);

		mavenModel.setBuild(new Build());
		mavenModel.getBuild().addPlugin(dependencyPlugin);
	}

}
