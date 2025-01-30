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
package net.codecadenza.eclipse.ui.operation;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormBuildConfiguration;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.service.form.init.GridPanelInitService;
import net.codecadenza.eclipse.service.form.init.UpdateFormInitService;
import net.codecadenza.eclipse.service.form.init.ViewFormInitService;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Display;

/**
 * <p>
 * Utility class for creating default forms of a domain object based on the provided configuration
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DefaultFormBuildOperation implements IRunnableWithProgress {
	private final FormBuildConfiguration configuration;
	private final FormGroup formGroup;
	private final Project project;

	/**
	 * Constructor
	 * @param formGroup
	 * @param configuration
	 */
	public DefaultFormBuildOperation(FormGroup formGroup, FormBuildConfiguration configuration) {
		this.formGroup = formGroup;
		this.configuration = configuration;
		this.project = formGroup.findProject();
	}

	/**
	 * Create and initialize an association helper for the given domain object
	 * @param domainObject the domain object to be analyze
	 * @return the initialized association helper object
	 */
	private AssociationHelper initAssociationHelper(DomainObject domainObject) {
		final var rootAssociation = new AssociationHelper(AssociationHelper.INITIAL_ALIAS, 0, domainObject.getName(), null);

		// Iterate over all associations in order to add respective helper objects to the root object
		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
			if (assoc instanceof OneToManyAssociation || assoc instanceof ManyToManyAssociation)
				continue;

			final var helper = new AssociationHelper(rootAssociation.getAlias(), rootAssociation.getCounter(), assoc.getName(), assoc,
					rootAssociation);
			boolean isOptional = false;

			// Don't add recursive associations!
			if (assoc.getTarget().equals(domainObject))
				continue;

			if (assoc instanceof final OneToOneAssociation oneToOne)
				isOptional = oneToOne.isOptional();
			else {
				final var manyToOne = (ManyToOneAssociation) assoc;
				isOptional = manyToOne.isOptional();
			}

			helper.setOuterJoin(isOptional);
			helper.nextAlias();
			helper.checkAlias();

			rootAssociation.getChildren().add(helper);
		}

		return rootAssociation;
	}

	/**
	 * Display an error in a message dialog
	 * @param e
	 */
	private void displayError(Exception e) {
		Display.getDefault().syncExec(() -> CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
		final DomainObject domainObject = configuration.getDomainObject();
		final var formService = new FormService(project);
		Namespace dtoNamespace = null;
		int work = 1;

		monitor.beginTask("Create selected default forms", configuration.getNumberOfObjectsToBeProcessed());

		if (configuration.isCreateGridPanels()) {
			// Iterate over all associations provided by this configuration in order to create grid panels
			for (final AbstractDomainAssociation assoc : configuration.getGridPanelAssociations()) {
				// We assume that the configuration provides only associations for grid panels that don't exist yet!
				final AssociationHelper assocHelper = initAssociationHelper(assoc.getTarget());
				final var initService = new GridPanelInitService(assoc, assocHelper);
				final FormPanel gridPanel = initService.initializeGridPanel();
				final Map<String, DomainAttribute> downloadAttrMap = initService.getDownloadAttrMap();
				final var boundaryMethodName = "get" + assoc.getUpperCaseName() + "Of" + assoc.getDomainObject().getName();
				final String queryStatement = initService.generateSelectStatement(true);

				// Determine the DTO namespace
				for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
					if (ns.getName().equals(gridPanel.getDTO().getDomainObject().getNamespace().getName()))
						dtoNamespace = ns;

				gridPanel.setFormGroup(formGroup);

				try {
					formService.createAssociationPanel(gridPanel, assoc, boundaryMethodName, dtoNamespace, queryStatement, "",
							downloadAttrMap);
				}
				catch (final Exception e) {
					displayError(e);
				}

				monitor.worked(work++);
			}
		}

		if (configuration.isCreateUpdateForm()) {
			final var initService = new UpdateFormInitService(domainObject, formGroup, FormTypeEnumeration.UPDATE,
					configuration.isShareDTOs());
			final Form form = initService.initializeForm();
			final Map<String, DTOBean> listDTOMap = initService.getListDTOMap();
			final Map<DTOBean, Namespace> namespaceMap = initService.getNamespaceMap();
			final var boundaryMethodName = "update" + domainObject.getName();

			try {
				formService.createUpdateForm(form, listDTOMap, form.getRoles(), formGroup, namespaceMap, new HashMap<>(),
						boundaryMethodName, true);
			}
			catch (final Exception e) {
				displayError(e);
			}

			monitor.worked(work++);
		}

		if (configuration.isCreateReadOnlyForm()) {
			final var initService = new UpdateFormInitService(domainObject, formGroup, FormTypeEnumeration.READONLY,
					configuration.isShareDTOs());
			final Form form = initService.initializeForm();
			final Map<String, DTOBean> listDTOMap = initService.getListDTOMap();
			final Map<DTOBean, Namespace> namespaceMap = initService.getNamespaceMap();
			final var boundaryMethodName = "find" + domainObject.getName() + "ById";

			try {
				formService.createUpdateForm(form, listDTOMap, form.getRoles(), formGroup, namespaceMap, new HashMap<>(),
						boundaryMethodName, true);
			}
			catch (final Exception e) {
				displayError(e);
			}

			monitor.worked(work++);
		}

		if (configuration.getCreateFormType() != null) {
			final var initService = new UpdateFormInitService(domainObject, formGroup, configuration.getCreateFormType(),
					configuration.isShareDTOs());
			final Form form = initService.initializeForm();
			final Map<String, DTOBean> listDTOMap = initService.getListDTOMap();
			final Map<DTOBean, Namespace> namespaceMap = initService.getNamespaceMap();
			final var boundaryMethodName = "create" + domainObject.getName();

			try {
				formService.createUpdateForm(form, listDTOMap, form.getRoles(), formGroup, namespaceMap, new HashMap<>(),
						boundaryMethodName, false);
			}
			catch (final Exception e) {
				displayError(e);
			}

			monitor.worked(work++);
		}

		if (configuration.getViewFormType() != null) {
			final AssociationHelper assocHelper = initAssociationHelper(domainObject);
			final var initService = new ViewFormInitService(domainObject, formGroup, configuration.getViewFormType(),
					BoundaryMethodDataFetchType.DEFAULT, assocHelper);
			final Form form = initService.initializeForm();
			final Map<String, DomainAttribute> downloadMap = initService.getDownloadAttrMap();
			final var countMethodName = "countAll" + domainObject.getNamePlural().substring(0, 1).toUpperCase()
					+ domainObject.getNamePlural().substring(1);
			final var boundaryMethodName = "searchAll" + domainObject.getNamePlural().substring(0, 1).toUpperCase()
					+ domainObject.getNamePlural().substring(1);
			final BoundaryMethodDataFetchType fetchType = BoundaryMethodDataFetchType.DEFAULT;
			final String dtoName = form.getDTO().getName();
			final String queryStatement = initService.generateSelectStatement(true);

			// Determine the DTO namespace
			for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
				if (ns.getName().equals(form.getDomainObject().getNamespace().getName()))
					dtoNamespace = ns;

			form.setFormGroup(formGroup);

			try {
				formService.createViewForm(form, boundaryMethodName, countMethodName, dtoName, dtoNamespace, queryStatement, "",
						downloadMap, fetchType);
			}
			catch (final Exception e) {
				displayError(e);
			}

			monitor.worked(work++);
		}

		if (configuration.isCreateLOV()) {
			final AssociationHelper assocHelper = initAssociationHelper(domainObject);
			final var initService = new ViewFormInitService(domainObject, formGroup, FormTypeEnumeration.LOV,
					BoundaryMethodDataFetchType.DEFAULT, assocHelper);
			final Form form = initService.initializeForm();
			final var boundaryMethodName = "get" + domainObject.getNamePlural().substring(0, 1).toUpperCase()
					+ domainObject.getNamePlural().substring(1) + "List";
			final BoundaryMethodDataFetchType fetchType = BoundaryMethodDataFetchType.DEFAULT;
			final String dtoName = form.getDTO().getName();
			final String queryStatement = initService.generateSelectStatement(true);

			// Determine the DTO namespace
			for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
				if (ns.getName().equals(form.getDomainObject().getNamespace().getName()))
					dtoNamespace = ns;

			form.setFormGroup(formGroup);

			try {
				formService.createViewForm(form, boundaryMethodName, null, dtoName, dtoNamespace, queryStatement, "", new HashMap<>(),
						fetchType);
			}
			catch (final Exception e) {
				displayError(e);
			}

			monitor.worked(work);
		}

		monitor.done();
	}

}
