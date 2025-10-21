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
package net.codecadenza.eclipse.service.build.imp;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.imp.base.AbstractProjectBuildService;

/**
 * <p>
 * Project build service for Maven
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MavenProjectBuildService extends AbstractProjectBuildService {
	private static final String OUTPUT_LOCATION = "target/classes";

	/**
	 * Constructor
	 * @param project
	 */
	public MavenProjectBuildService(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#getDefaultBuildConfiguration()
	 */
	@Override
	public List<BuildArtifact> getDefaultBuildConfiguration() {
		final var buildConfig = new ArrayList<BuildArtifact>();
		buildConfig.add(createDefaultRootArtifact(BuildArtifactType.MASTER));
		buildConfig.add(createDefaultArtifact(BuildArtifactType.DOMAIN));
		buildConfig.add(createDefaultArtifact(BuildArtifactType.DTO));
		buildConfig.add(createDefaultArtifact(BuildArtifactType.SHARED));
		buildConfig.add(createDefaultArtifact(BuildArtifactType.DATA_EXCHANGE));

		if (project.isBoundaryMode()) {
			buildConfig.add(createDefaultArtifact(BuildArtifactType.REPOSITORY));
			buildConfig.add(createDefaultArtifact(BuildArtifactType.BOUNDARY));

			if (project.isAddBoundaryInterface())
				buildConfig.add(createDefaultArtifact(BuildArtifactType.CLIENT_INTERFACE));

			if (project.hasClient() && !project.hasJSFOrVaadinClient() && !project.isJavaSEApplication())
				buildConfig.add(createDefaultArtifact(BuildArtifactType.SERVER));
		}
		else
			buildConfig.add(createDefaultArtifact(BuildArtifactType.FACADE));

		if (!project.isJavaSEApplication()) {
			buildConfig.add(createDefaultArtifact(BuildArtifactType.SERVICE));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_SEI_SOAP, IntegrationTechnology.SOAP + INTEGR_SUFFIX_SEI));
			buildConfig.add(
					createDefaultArtifact(BuildArtifactType.INTEGRATION_CLIENT_SOAP, IntegrationTechnology.SOAP + INTEGR_SUFFIX_CLIENT));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_IMP_SOAP, IntegrationTechnology.SOAP + INTEGR_SUFFIX_IMP));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_TEST_SOAP, IntegrationTechnology.SOAP + INTEGR_TEST_SUFFIX));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_SEI_REST, IntegrationTechnology.REST + INTEGR_SUFFIX_SEI));
			buildConfig.add(
					createDefaultArtifact(BuildArtifactType.INTEGRATION_CLIENT_REST, IntegrationTechnology.REST + INTEGR_SUFFIX_CLIENT));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_IMP_REST, IntegrationTechnology.REST + INTEGR_SUFFIX_IMP));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_TEST_REST, IntegrationTechnology.REST + INTEGR_TEST_SUFFIX));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_SEI_JMS, IntegrationTechnology.JMS + INTEGR_SUFFIX_SEI));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_CLIENT_JMS, IntegrationTechnology.JMS + INTEGR_SUFFIX_CLIENT));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_IMP_JMS, IntegrationTechnology.JMS + INTEGR_SUFFIX_IMP));
			buildConfig
					.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_TEST_JMS, IntegrationTechnology.JMS + INTEGR_TEST_SUFFIX));

			if (project.isJakartaEEApplication()) {
				buildConfig
						.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_SEI_RMI, IntegrationTechnology.RMI + INTEGR_SUFFIX_SEI));
				buildConfig.add(
						createDefaultArtifact(BuildArtifactType.INTEGRATION_CLIENT_RMI, IntegrationTechnology.RMI + INTEGR_SUFFIX_CLIENT));
				buildConfig
						.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_IMP_RMI, IntegrationTechnology.RMI + INTEGR_SUFFIX_IMP));
				buildConfig
						.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_TEST_RMI, IntegrationTechnology.RMI + INTEGR_TEST_SUFFIX));
			}
			else if (project.isBoundaryMode()) {
				buildConfig
						.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_SEI_KAFKA, IntegrationTechnology.KAFKA + INTEGR_SUFFIX_SEI));
				buildConfig.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_CLIENT_KAFKA,
						IntegrationTechnology.KAFKA + INTEGR_SUFFIX_CLIENT));
				buildConfig
						.add(createDefaultArtifact(BuildArtifactType.INTEGRATION_IMP_KAFKA, IntegrationTechnology.KAFKA + INTEGR_SUFFIX_IMP));
				buildConfig.add(
						createDefaultArtifact(BuildArtifactType.INTEGRATION_TEST_KAFKA, IntegrationTechnology.KAFKA + INTEGR_TEST_SUFFIX));
			}
		}

		if (project.hasClient())
			buildConfig.add(createDefaultArtifact(BuildArtifactType.GUI));
		else
			buildConfig.add(createDefaultArtifact(BuildArtifactType.SERVER));

		if (project.hasAngularClient() || project.hasJSFOrVaadinClient())
			buildConfig.add(createDefaultArtifact(BuildArtifactType.SELENIUM_TEST));

		return buildConfig;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.base.AbstractProjectBuildService#getOutputLocation()
	 */
	@Override
	protected String getOutputLocation() {
		return OUTPUT_LOCATION;
	}

}
