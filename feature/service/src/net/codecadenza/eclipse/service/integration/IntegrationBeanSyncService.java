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
package net.codecadenza.eclipse.service.integration;

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationFactory;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.ecore.resource.Resource;

/**
 * <p>
 * Service for automatically adding integration beans and integration methods. The synchronization is only essential for Angular
 * applications. In case of other technologies the integration objects must be created manually!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationBeanSyncService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public IntegrationBeanSyncService(Project project) {
		this.project = project;
	}

	/**
	 * Search for new integration beans that should be created or add new methods to existing beans
	 * @param rebuild flag that controls if the respective source files should be rebuilt
	 * @throws IllegalStateException if the project doesn't contain a REST integration module
	 * @throws Exception if an internal error has occurred
	 */
	public void sync(boolean rebuild) throws Exception {
		if (!project.hasAngularClient())
			return;

		final IntegrationModule module = project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_REST);

		if (module == null)
			throw new IllegalStateException("The REST module could not be found!");

		for (final DomainObject domainObject : project.getAllDomainObjectsOfProject(false, true)) {
			// Test if an integration bean for this domain object already exists!
			final AbstractIntegrationBean integrationBean = project.searchIntegrationBean(IntegrationTechnology.REST, domainObject);

			if (integrationBean == null) {
				final BoundaryBean boundary = project.getBoundaryByDomainObject(domainObject);

				// Do not add a new integration bean if no corresponding boundary bean exists yet!
				if (boundary == null)
					continue;

				final AbstractIntegrationBean newIntegrationBean = createNewIntegrationBean(domainObject, module);

				if (rebuild)
					new IntegrationBeanService(project).rebuildIntegrationBeanSourceFiles(newIntegrationBean);
			}
			else {
				// Correct all improper input and output content types
				integrationBean.getMethods().forEach(integrationMethod -> {
					final var restMethod = (RESTIntegrationMethod) integrationMethod;

					if (restMethod.getOutputType() == MediaTypeEnumeration.XML)
						restMethod.setOutputType(MediaTypeEnumeration.JSON);

					if (restMethod.getInputType() == MediaTypeEnumeration.XML)
						restMethod.setInputType(MediaTypeEnumeration.JSON);
				});

				if (addNewMethods(integrationBean) && rebuild)
					new IntegrationBeanService(project).rebuildIntegrationBeanSourceFiles(integrationBean);
			}
		}
	}

	/**
	 * Search for new integration beans that should be created or add new methods to existing beans
	 * @throws IllegalStateException if the project doesn't contain a REST integration module
	 * @throws Exception if an internal error has occurred
	 */
	public void sync() throws Exception {
		sync(true);
	}

	/**
	 * Create and initialize a new integration bean
	 * @param domainObject
	 * @param module
	 * @return the generated integration bean
	 */
	private AbstractIntegrationBean createNewIntegrationBean(DomainObject domainObject, IntegrationModule module) {
		final RESTIntegrationBean restBean = IntegrationFactory.eINSTANCE.createRESTIntegrationBean();
		final var beanName = domainObject.getName() + module.getTechnology().getName() + "ServiceBean";
		final var interfaceName = domainObject.getName() + module.getTechnology().getName() + "Service";
		final var comment = module.getTechnology().getName() + " service for " + domainObject.getLabel() + " objects";

		restBean.setDomainObject(domainObject);
		restBean.setNamespace(module.getNamespace());
		restBean.setName(beanName);
		restBean.setComment(comment);
		restBean.setInterfaceName(interfaceName);
		restBean.setPath(domainObject.getName().toLowerCase());

		if (module.hasClientArtifact())
			restBean.setClientClassName(domainObject.getName() + module.getTechnology().getName() + "Client");

		if (module.isAddProducers())
			restBean.setProducerClassName(domainObject.getName() + module.getTechnology().getName() + "Producer");

		addNewMethods(restBean);

		final Resource eResource = module.getNamespace().eResource();

		if (!eResource.getContents().contains(restBean))
			eResource.getContents().add(restBean);

		return restBean;
	}

	/**
	 * Add new methods to the given integration bean
	 * @param integrationBean
	 * @return true if one method has been added at least
	 */
	private boolean addNewMethods(AbstractIntegrationBean integrationBean) {
		final BoundaryBean boundary = project.getBoundaryByDomainObject(integrationBean.getDomainObject());
		boolean newMethodAdded = false;

		for (final BoundaryMethod boundaryMethod : boundary.getBoundaryMethods()) {
			final boolean found = integrationBean.getMethods().stream()
					.anyMatch(integrationMethod -> integrationMethod.getBoundaryMethod().equals(boundaryMethod));

			if (!found) {
				final var initService = new IntegrationMethodInitService();
				final AbstractIntegrationMethod integrationMethod = initService.initializeMethod(integrationBean, boundaryMethod,
						MediaTypeEnumeration.JSON);

				integrationBean.getMethods().add(integrationMethod);

				newMethodAdded = true;
			}
		}

		return newMethodAdded;
	}

}
