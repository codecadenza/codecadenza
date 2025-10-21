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

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.resource.dependency.DependencyConfigMappingType;
import net.codecadenza.eclipse.resource.dependency.DependencyMappingType;
import net.codecadenza.eclipse.resource.dependency.FilterMappingType;

/**
 * <p>
 * Service for resolving all dependencies of a given artifact
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DependencyService {
	private static final String PROJECT_VERSION = "${project.version}";
	private static final String FILTER_ALL = "ALL";
	private static final String SCOPE_COMPILE = "compile";
	private final BuildArtifact buildArtifact;
	private final Project project;

	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public DependencyService(BuildArtifact buildArtifact) {
		this.buildArtifact = buildArtifact;
		this.project = buildArtifact.getProject();
	}

	/**
	 * @param artifactType
	 * @return a list of dependencies for the given artifact type
	 * @throws Exception if reading of dependency configuration has failed
	 */
	public List<Dependency> getDependencies(BuildArtifactType artifactType) throws Exception {
		final var dependencies = new ArrayList<Dependency>();
		final var filters = new ArrayList<FilterMappingType>();
		final List<DependencyConfigMappingType> configuration = CodeCadenzaResourcePlugin.getDependencyConfiguration();

		for (final DependencyConfigMappingType config : configuration)
			if (config.getBuildType() == artifactType) {
				filters.addAll(config.getFilters());
				break;
			}

		for (final FilterMappingType filter : filters) {
			boolean addDependencies = true;

			if (!filter.getClientPlatform().equals(FILTER_ALL))
				if (project.getClientPlatform().name().equals(filter.getClientPlatform()))
					addDependencies = true;
				else
					addDependencies = false;

			if (addDependencies && !filter.getJpaProvider().equals(FILTER_ALL))
				if (project.getPersistenceProvider().name().equals(filter.getJpaProvider()))
					addDependencies = true;
				else
					addDependencies = false;

			if (addDependencies && !filter.getServerPlatform().equals(FILTER_ALL))
				if (project.getServerPlatform().name().equals(filter.getServerPlatform()))
					addDependencies = true;
				else
					addDependencies = false;

			if (addDependencies && !filter.getTechnology().equals(FILTER_ALL))
				if (project.getTechnology().name().equals(filter.getTechnology()))
					addDependencies = true;
				else
					addDependencies = false;

			if (addDependencies && !filter.getValidationType().equals(FILTER_ALL))
				if (project.getValidationType().name().equals(filter.getValidationType()))
					addDependencies = true;
				else
					addDependencies = false;

			if (addDependencies && !filter.getDatabase().equals(FILTER_ALL))
				if (project.getDatabase().getVendorGroup().name().equals(filter.getDatabase())) {
					// The GUI artifact won't need a JDBC driver if the application provides a back-end!
					if (artifactType == BuildArtifactType.GUI && !project.isJavaSEApplication()
							&& (project.hasSwingClient() || project.hasJavaFXClient() || project.hasEclipseClient()))
						addDependencies = false;
				}
				else
					addDependencies = false;

			if (!addDependencies)
				continue;

			for (final DependencyMappingType mappedDependency : filter.getDependencies()) {
				final var dependency = new Dependency();
				dependency.setGroupName(mappedDependency.getGroupName());
				dependency.setName(mappedDependency.getName());
				dependency.setScope(mappedDependency.getScope());
				dependency.setVersion(mappedDependency.getVersion());
				dependency.setRepositoryId(mappedDependency.getRepositoryId());
				dependency.setRepositoryURL(mappedDependency.getRepositoryURL());
				dependency.setType(mappedDependency.getType());

				mappedDependency.getExclusions().forEach(mappedExcludedDependency -> {
					final var excludedDependency = new Dependency();
					excludedDependency.setGroupName(mappedExcludedDependency.getGroupName());
					excludedDependency.setName(mappedExcludedDependency.getName());
					excludedDependency.setScope(mappedExcludedDependency.getScope());
					excludedDependency.setVersion(mappedExcludedDependency.getVersion());
					excludedDependency.setRepositoryId(mappedExcludedDependency.getRepositoryId());
					excludedDependency.setRepositoryURL(mappedExcludedDependency.getRepositoryURL());
					excludedDependency.setType(mappedExcludedDependency.getType());

					dependency.getExclusions().add(excludedDependency);
				});

				dependencies.add(dependency);
			}
		}

		return dependencies;
	}

	/**
	 * @return a list containing all dependencies of this artifact
	 * @throws Exception if reading of dependency configuration has failed
	 */
	public List<Dependency> getDependencies() throws Exception {
		final Map<BuildArtifactType, List<BuildArtifactType>> moduleDependencies = getModuleDependencies();
		final List<Dependency> dependencies = getDependencies(buildArtifact.getType());

		for (final BuildArtifactType type : BuildArtifactType.values()) {
			if (type == BuildArtifactType.MASTER || buildArtifact.getType() == type)
				continue;

			if (buildArtifact.getContainedArtifacts().contains(type)) {
				for (final Dependency containedDependency : getDependencies(type)) {
					boolean addContainedDependency = true;

					for (final Dependency dependency : dependencies)
						if (dependency.equals(containedDependency)) {
							addContainedDependency = false;
							break;
						}
						else if (dependency.isSameDependency(containedDependency)) {
							if (containedDependency.getScope().equals(SCOPE_COMPILE))
								dependency.setScope(containedDependency.getScope());

							addContainedDependency = false;
							break;
						}

					if (addContainedDependency)
						dependencies.add(containedDependency);
				}
			}
			else {
				// Omit module dependencies for all projects that aren't based on Maven!
				if (buildArtifact.getType() == BuildArtifactType.GUI && project.hasEclipseClient())
					continue;

				// Check if this artifact depends on a module of this type!
				if (!moduleDependencies.containsKey(buildArtifact.getType()))
					continue;

				if (moduleDependencies.get(buildArtifact.getType()).contains(type)) {
					// Add all module dependencies
					for (final BuildArtifact artifact : project.getBuildConfiguration()) {
						if (artifact.getType() == BuildArtifactType.MASTER)
							continue;

						if (artifact.getType() != type)
							continue;

						final var dependency = new Dependency();
						dependency.setGroupName(project.getRootNamespace().toString());
						dependency.setName(artifact.getName());
						dependency.setVersion(PROJECT_VERSION);
						dependency.setScope(SCOPE_COMPILE);

						dependencies.add(dependency);
					}
				}
			}
		}

		return dependencies;
	}

	/**
	 * @return a map containing all module dependencies
	 */
	private Map<BuildArtifactType, List<BuildArtifactType>> getModuleDependencies() {
		final var moduleDependencyMap = new EnumMap<BuildArtifactType, List<BuildArtifactType>>(BuildArtifactType.class);

		for (final BuildArtifactType type : BuildArtifactType.values()) {
			if (type == BuildArtifactType.REPOSITORY) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.DOMAIN);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.DOMAIN || type == BuildArtifactType.DTO) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.SHARED);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.BOUNDARY) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.REPOSITORY);
				dependencies.add(BuildArtifactType.DOMAIN);
				dependencies.add(BuildArtifactType.DTO);
				dependencies.add(BuildArtifactType.DATA_EXCHANGE);

				if (project.isAddBoundaryInterface())
					dependencies.add(BuildArtifactType.CLIENT_INTERFACE);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.FACADE) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.DOMAIN);
				dependencies.add(BuildArtifactType.DTO);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.CLIENT_INTERFACE) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.DTO);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.DATA_EXCHANGE) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.DOMAIN);
				dependencies.add(BuildArtifactType.DTO);

				if (project.isBoundaryMode())
					dependencies.add(BuildArtifactType.REPOSITORY);
				else
					dependencies.add(BuildArtifactType.FACADE);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.SERVICE) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.DOMAIN);
				dependencies.add(BuildArtifactType.REPOSITORY);
				dependencies.add(BuildArtifactType.DTO);

				if (project.isSpringBootApplication())
					if (project.isBoundaryMode())
						dependencies.add(BuildArtifactType.BOUNDARY);
					else
						dependencies.add(BuildArtifactType.FACADE);

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.GUI) {
				final var dependencies = new ArrayList<BuildArtifactType>();

				if (project.isJavaSEApplication() || project.hasJSFOrVaadinClient()) {
					if (project.isBoundaryMode())
						dependencies.add(BuildArtifactType.BOUNDARY);
					else {
						dependencies.add(BuildArtifactType.FACADE);
						dependencies.add(BuildArtifactType.DATA_EXCHANGE);
					}
				}
				else {
					// A rich-client GUI only depends on the DTO and the client interface artifact!
					dependencies.add(BuildArtifactType.DTO);
					dependencies.add(BuildArtifactType.CLIENT_INTERFACE);
				}

				if (project.hasJSFOrVaadinClient()) {
					dependencies.add(BuildArtifactType.SERVICE);
					dependencies.add(BuildArtifactType.INTEGRATION_IMP_SOAP);
					dependencies.add(BuildArtifactType.INTEGRATION_IMP_REST);
					dependencies.add(BuildArtifactType.INTEGRATION_IMP_JMS);

					if (project.isJakartaEEApplication())
						dependencies.add(BuildArtifactType.INTEGRATION_IMP_RMI);
					else if (project.isBoundaryMode())
						dependencies.add(BuildArtifactType.INTEGRATION_IMP_KAFKA);
				}

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.SERVER) {
				final var dependencies = new ArrayList<BuildArtifactType>();
				dependencies.add(BuildArtifactType.SERVICE);
				dependencies.add(BuildArtifactType.INTEGRATION_IMP_SOAP);
				dependencies.add(BuildArtifactType.INTEGRATION_IMP_REST);
				dependencies.add(BuildArtifactType.INTEGRATION_IMP_JMS);

				if (project.isJakartaEEApplication())
					dependencies.add(BuildArtifactType.INTEGRATION_IMP_RMI);

				if (project.isBoundaryMode()) {
					dependencies.add(BuildArtifactType.BOUNDARY);

					if (project.isSpringBootApplication())
						dependencies.add(BuildArtifactType.INTEGRATION_IMP_KAFKA);
				}
				else {
					dependencies.add(BuildArtifactType.FACADE);
					dependencies.add(BuildArtifactType.DATA_EXCHANGE);
				}

				moduleDependencyMap.put(type, dependencies);
			}
			else if (type == BuildArtifactType.INTEGRATION_CLIENT_SOAP)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_SOAP));
			else if (type == BuildArtifactType.INTEGRATION_CLIENT_REST)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_REST));
			else if (type == BuildArtifactType.INTEGRATION_CLIENT_RMI)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_RMI));
			else if (type == BuildArtifactType.INTEGRATION_CLIENT_KAFKA)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_KAFKA));
			else if (type == BuildArtifactType.INTEGRATION_CLIENT_JMS)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_JMS));
			else if (type == BuildArtifactType.INTEGRATION_SEI_SOAP || type == BuildArtifactType.INTEGRATION_SEI_REST
					|| type == BuildArtifactType.INTEGRATION_SEI_RMI || type == BuildArtifactType.INTEGRATION_SEI_JMS)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.DTO));
			else if (type == BuildArtifactType.INTEGRATION_IMP_SOAP) {
				if (project.isBoundaryMode())
					moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_SOAP, BuildArtifactType.BOUNDARY));
				else
					moduleDependencyMap.put(type,
							List.of(BuildArtifactType.INTEGRATION_SEI_SOAP, BuildArtifactType.FACADE, BuildArtifactType.DATA_EXCHANGE));
			}
			else if (type == BuildArtifactType.INTEGRATION_IMP_REST || type == BuildArtifactType.INTEGRATION_IMP_JMS) {
				if (project.isBoundaryMode())
					moduleDependencyMap.put(type, List.of(BuildArtifactType.BOUNDARY));
				else
					moduleDependencyMap.put(type, List.of(BuildArtifactType.FACADE, BuildArtifactType.DATA_EXCHANGE));
			}
			else if (type == BuildArtifactType.INTEGRATION_IMP_RMI) {
				if (project.isBoundaryMode())
					moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_RMI, BuildArtifactType.BOUNDARY));
				else
					moduleDependencyMap.put(type,
							List.of(BuildArtifactType.INTEGRATION_SEI_RMI, BuildArtifactType.FACADE, BuildArtifactType.DATA_EXCHANGE));
			}
			else if (type == BuildArtifactType.INTEGRATION_IMP_KAFKA)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_SEI_KAFKA, BuildArtifactType.BOUNDARY));
			else if (type == BuildArtifactType.INTEGRATION_TEST_SOAP)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_CLIENT_SOAP));
			else if (type == BuildArtifactType.INTEGRATION_TEST_REST)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_CLIENT_REST));
			else if (type == BuildArtifactType.INTEGRATION_TEST_RMI)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_CLIENT_RMI));
			else if (type == BuildArtifactType.INTEGRATION_TEST_KAFKA)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_CLIENT_KAFKA));
			else if (type == BuildArtifactType.INTEGRATION_TEST_JMS)
				moduleDependencyMap.put(type, List.of(BuildArtifactType.INTEGRATION_CLIENT_JMS));
		}

		return moduleDependencyMap;
	}

}
