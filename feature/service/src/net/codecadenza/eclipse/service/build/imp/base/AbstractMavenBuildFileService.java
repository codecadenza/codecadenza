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
package net.codecadenza.eclipse.service.build.imp.base;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.imp.dependency.DependencyService;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Exclusion;
import org.apache.maven.model.Model;
import org.apache.maven.model.Repository;
import org.codehaus.plexus.util.xml.Xpp3Dom;

/**
 * <p>
 * Abstract base class for services that create a Maven build file based on a given artifact
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AbstractMavenBuildFileService {
	protected static final String GROUP_ID = "groupId";
	protected static final String ARTIFACT_ID = "artifactId";
	protected static final String VERSION = "version";
	protected static final String REF_GROUP_ID = "${project.groupId}";
	protected static final String REF_ARTIFACT_ID = "${project.artifactId}";
	protected static final String REF_VERSION = "${project.version}";
	protected static final String REF_SOURCE_DIR = "${project.build.sourceDirectory}";
	protected static final String TYPE = "type";
	protected static final String GOAL_COPY = "copy";
	protected static final String GOAL_DEPLOY = "deploy";
	protected static final String PHASE_INSTALL = "install";
	protected static final String PHASE_GENERATE_SOURCES = "generate-sources";
	protected static final String DEFAULT_VERSION = "1.0.0";
	protected static final String MODEL_VERSION = "4.0.0";
	protected static final String PACKAGING_TYPE_POM = "pom";
	protected static final String PACKAGING_TYPE_WAR = "war";
	protected static final String PACKAGING_TYPE_JAR = "jar";
	protected static final String CONFIGURATION_ELEMENT = "configuration";
	protected static final String SCOPE_TEST = "test";
	protected static final String SCOPE_IMPORT = "import";
	protected static final String INCLUDE_ALL_FILES = "**/*";
	protected static final String INCLUDE_PROPERTY_FILES = "**/*.properties";

	protected final BuildArtifact artifact;
	protected final Model mavenModel;
	protected final Project project;

	/**
	 * Constructor
	 * @param artifact
	 */
	public AbstractMavenBuildFileService(BuildArtifact artifact) {
		this.artifact = artifact;
		this.project = artifact.getProject();

		this.mavenModel = new Model();
		this.mavenModel.setModelVersion(MODEL_VERSION);
	}

	/**
	 * Get all dependencies
	 * @return a list with all dependencies
	 * @throws Exception if the dependencies could not be determined
	 */
	protected List<Dependency> getDependencies() throws Exception {
		final var dependencies = new ArrayList<Dependency>();

		new DependencyService(artifact).getDependencies().forEach(dependency -> {
			final var mavenDependency = new Dependency();
			mavenDependency.setArtifactId(dependency.getName());
			mavenDependency.setGroupId(dependency.getGroupName());
			mavenDependency.setScope(dependency.getScope());
			mavenDependency.setVersion(dependency.getVersion());

			if (dependency.getType() != null)
				mavenDependency.setType(dependency.getType());

			if (dependency.getRepositoryId() != null) {
				final var repository = new Repository();
				repository.setId(dependency.getRepositoryId());
				repository.setUrl(dependency.getRepositoryURL());

				mavenModel.getRepositories().add(repository);
			}

			dependency.getExclusions().forEach(excludedDependency -> {
				final var exclusion = new Exclusion();
				exclusion.setArtifactId(excludedDependency.getName());
				exclusion.setGroupId(excludedDependency.getGroupName());

				mavenDependency.getExclusions().add(exclusion);
			});

			dependencies.add(mavenDependency);
		});

		return dependencies;
	}

	/**
	 * Add a new element to the given parent element
	 * @param parentElement
	 * @param name
	 * @param value
	 * @return the created element
	 */
	protected Xpp3Dom addElement(Xpp3Dom parentElement, String name, String value) {
		final var element = new Xpp3Dom(name);

		if (value != null)
			element.setValue(value);

		parentElement.addChild(element);

		return element;
	}

}
