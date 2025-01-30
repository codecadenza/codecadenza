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
package net.codecadenza.eclipse.service.form;

import static net.codecadenza.eclipse.shared.Constants.ACTION_ADD;
import static net.codecadenza.eclipse.shared.Constants.ACTION_CREATE;
import static net.codecadenza.eclipse.shared.Constants.ACTION_GET;
import static net.codecadenza.eclipse.shared.Constants.ACTION_UPDATE;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_DELETE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.FormGeneratorFactory;
import net.codecadenza.eclipse.generator.client.IFormGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.module.AngularFormModuleGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.service.mapping.MappingObjectService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Service for forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormService {
	private final Project project;
	private final BoundaryService boundaryService;
	private final RepositoryService repositoryService;
	private final DTOBeanService dtoService;
	private final MappingObjectService mappingObjectService;
	private final GUITestCaseService guiTestCaseService;
	private final IntegrationBeanSyncService integrationSyncService;

	/**
	 * Constructor
	 * @param project
	 */
	public FormService(Project project) {
		this.project = project;
		this.boundaryService = new BoundaryService(project);
		this.repositoryService = new RepositoryService(project);
		this.dtoService = new DTOBeanService(project);
		this.mappingObjectService = new MappingObjectService(project);
		this.guiTestCaseService = new GUITestCaseService(project);
		this.integrationSyncService = new IntegrationBeanSyncService(project);
	}

	/**
	 * Rebuild the given form
	 * @param form
	 * @throws Exception if the rebuild operation has failed
	 */
	public void rebuildForm(Form form) throws Exception {
		final IFormGenerator formGenerator = FormGeneratorFactory.getFormGenerator(project);
		formGenerator.createForm(form);
	}

	/**
	 * Rebuild the given grid panel
	 * @param gridPanel
	 * @throws Exception if the rebuild operation has failed
	 */
	public void rebuildGridPanel(FormPanel gridPanel) throws Exception {
		final IFormGenerator formGenerator = FormGeneratorFactory.getFormGenerator(project);
		formGenerator.createGridPanel(gridPanel);
	}

	/**
	 * Rebuild the application tree navigator
	 * @throws Exception if the rebuild operation has failed
	 */
	public void rebuildNavigator() throws Exception {
		final IFormGenerator formGenerator = FormGeneratorFactory.getFormGenerator(project);
		formGenerator.createNavigator(project);
	}

	/**
	 * Create a new view form
	 * @param form
	 * @param boundaryMethodName
	 * @param countMethodName
	 * @param dtoName
	 * @param dtoNamespace
	 * @param queryStatement
	 * @param custStatement
	 * @param downloadMap
	 * @param fetchType
	 * @throws IllegalStateException if the view form could not be created due to a validation error
	 * @throws Exception if an internal error has occurred
	 */
	public void createViewForm(Form form, String boundaryMethodName, String countMethodName, String dtoName, Namespace dtoNamespace,
			String queryStatement, String custStatement, Map<String, DomainAttribute> downloadMap,
			BoundaryMethodDataFetchType fetchType) throws Exception {
		final FormGroup formGroup = form.getFormGroup();
		final DomainObject domainObject = form.getDomainObject();
		final DTOBean formDTO = form.getDTO();
		formDTO.setName(dtoName);

		// Check if another class with the same name already exists
		final IStatus status = mappingObjectService.validateMappingObject(formDTO);

		if (!status.isOK())
			throw new IllegalStateException(status.getMessage());

		// Check if a view form with same name already exists
		for (final Form f : project.getAllFormsOfProject()) {
			final FormTypeEnumeration t = f.getFormType();

			if (t != FormTypeEnumeration.SEARCHABLE_VIEW && t != FormTypeEnumeration.SIMPLE_VIEW && t != FormTypeEnumeration.TREE_VIEW)
				continue;

			if (form.equals(f))
				continue;

			if (form.getName().equals(f.getName()))
				throw new IllegalStateException("A view with this name already exists!");
		}

		final BoundaryBean boundaryBean = boundaryService.getOrCreateBoundaryOfDomainObject(domainObject);

		// Create the boundary search method
		BoundaryMethod searchMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		searchMethod.setName(boundaryMethodName);
		searchMethod.setQueryStatement(queryStatement);
		searchMethod.setCustomStatement(custStatement);
		searchMethod.setDataFetchType(fetchType);

		RepositoryMethod repositoryMethod = boundaryBean.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.SEARCH);

		searchMethod.setServiceMethod(repositoryMethod);

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW) {
			searchMethod.setComment("Search for " + form.getDomainObject().getLabel() + " objects");
			searchMethod.setMethodType(BoundaryMethodTypeEnumeration.SEARCH);
		}
		else {
			searchMethod.setComment("Get a list of " + form.getDomainObject().getLabel() + " objects");
			searchMethod.setMethodType(BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES);

			// Add a filter parameter
			final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setName("filter");
			param.setMethod(searchMethod);
			param.setType(project.getJavaTypeByName(JavaType.STRING));
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			searchMethod.getMethodParameters().add(param);
		}

		searchMethod.setReturnType(formDTO);
		searchMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);

		BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(searchMethod, boundaryBean);

		if (existingMethod == null) {
			searchMethod.setBoundaryBean(boundaryBean);
			boundaryBean.getBoundaryMethods().add(searchMethod);
		}
		else
			searchMethod = existingMethod;

		// Add the count method for a searchable view!
		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			BoundaryMethod countMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();

			repositoryMethod = boundaryBean.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.COUNT);

			countMethod.setServiceMethod(repositoryMethod);
			countMethod.setName(countMethodName);
			countMethod.setComment("Count " + form.getDomainObject().getLabel() + " objects");
			countMethod.setMethodType(BoundaryMethodTypeEnumeration.COUNT);
			countMethod.setReturnType(project.getJavaTypeByName(JavaType.LONG));
			countMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			countMethod.setQueryStatement(queryStatement);
			countMethod.setCustomStatement(custStatement);
			countMethod.setDataFetchType(fetchType);

			existingMethod = boundaryService.checkBoundaryMethod(countMethod, boundaryBean);

			if (existingMethod == null) {
				countMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(countMethod);
			}
			else
				countMethod = existingMethod;

			// Attach the count method to the panel
			for (final FormPanel p : form.getFormPanels())
				p.setBoundaryMethod(countMethod);
		}

		// Set the form attribute of all actions
		form.getActions().forEach(a -> a.setForm(form));

		// Initialize the boundary methods for additional form actions
		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DELETE || a.getType() == ActionType.DOWNLOAD || a.getType() == ActionType.COPY)
				addBoundaryMethodToAction(a, boundaryBean, downloadMap);

		formGroup.getForms().add(form);
		form.getDTO().setNamespace(dtoNamespace);
		dtoNamespace.getJavaTypes().add(form.getDTO());
		form.setBoundaryMethod(searchMethod);

		project.eResource().getContents().add(form);

		// Save the form panels
		form.getFormPanels().forEach(p -> {
			if (!project.eResource().getContents().contains(p))
				project.eResource().getContents().add(p);

			if (p.getFormTable() != null) {
				final FormTable t = p.getFormTable();

				if (!project.eResource().getContents().contains(t))
					project.eResource().getContents().add(t);
			}
		});

		// Add the form DTO to the meta-model
		project.eResource().getContents().add(formDTO);

		// Save boundaries and repositories
		if (!project.eResource().getContents().contains(boundaryBean))
			project.eResource().getContents().add(boundaryBean);

		if (!project.eResource().getContents().contains(boundaryBean.getRepository()))
			project.eResource().getContents().add(boundaryBean.getRepository());

		// Synchronize integration beans
		integrationSyncService.sync();

		EclipseIDEService.saveProjectMetaData(project);

		// Build the DTO
		dtoService.rebuildDTOBeanSourceFiles(formDTO);

		// Build the boundary and the repository
		boundaryService.rebuildBoundarySourceFiles(boundaryBean);

		if (project.isBoundaryMode())
			repositoryService.rebuildRepositorySourceFiles(boundaryBean.getRepository());

		if (form.getFormType() != FormTypeEnumeration.LOV && project.hasEclipseClient()) {
			final String projectName = project.getTargetProjectName(BuildArtifactType.GUI);
			String packageName = project.getClientNamespace().toString();

			packageName += PACK_CLIENT_VIEW;

			// Add the view to the application model
			if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW || form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
				EclipseIDEService.addViewToApplicationModel(projectName, form.getTitle(), packageName + "." + form.getName());
		}

		// Build the form
		rebuildForm(form);

		// Synchronize GUI tests
		guiTestCaseService.syncOnCreateForm(form);
	}

	/**
	 * Create a new grid panel
	 * @param panel
	 * @param assoc
	 * @param methodName
	 * @param dtoNamespace
	 * @param queryStatement
	 * @param customStatement
	 * @param downloadMap
	 * @throws IllegalStateException if the grid panel could not be created due to a validation error
	 * @throws Exception if an internal error has occurred
	 */
	public void createAssociationPanel(FormPanel panel, AbstractDomainAssociation assoc, String methodName, Namespace dtoNamespace,
			String queryStatement, String customStatement, Map<String, DomainAttribute> downloadMap) throws Exception {
		final var dtoBuildList = new BasicEList<DTOBean>();
		final var boundaryBuildList = new BasicEList<BoundaryBean>();
		final FormGroup formGroup = panel.getFormGroup();
		final DTOBean panelDTO = panel.getDTO();
		final DomainObject domainObject = assoc.getDomainObject();
		// Check if another class with the same name already exists
		final IStatus status = mappingObjectService.validateMappingObject(panelDTO);

		if (!status.isOK())
			throw new IllegalStateException(status.getMessage());

		// Check if a panel with the same name already exists
		for (final FormPanel gridPanel : project.getAllGridPanelsOfProject()) {
			if (gridPanel.equals(panel))
				continue;

			if (panel.getName().equals(gridPanel.getName()))
				throw new IllegalStateException("A grid panel with this name already exists!");
		}

		dtoBuildList.add(panelDTO);
		panel.setAssociation(assoc);

		final BoundaryBean boundaryBean = boundaryService.getOrCreateBoundaryOfDomainObject(domainObject);
		boundaryBuildList.add(boundaryBean);

		final RepositoryMethod repositoryMethod = boundaryBean.getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.SEARCH);

		// Add a boundary method to fetch the data
		final BoundaryMethod listMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();

		listMethod.setName(methodName);
		listMethod
				.setComment("Get all " + panel.getDTO().getDomainObject().getLabelPlural() + " of a given " + domainObject.getLabel());
		listMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		listMethod.setReturnType(panel.getDTO());
		listMethod.setQueryStatement(queryStatement);
		listMethod.setCustomStatement(customStatement);
		listMethod.setMethodType(BoundaryMethodTypeEnumeration.FIND_BY_PARENT);
		listMethod.setServiceMethod(repositoryMethod);

		dtoBuildList.add(panel.getDTO());

		final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
		paramId.setName("id");
		paramId.setType(domainObject.getPKAttribute().getJavaType());
		paramId.setMethod(listMethod);

		listMethod.getMethodParameters().add(paramId);

		final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(listMethod, boundaryBean);

		if (existingMethod == null) {
			boundaryBean.getBoundaryMethods().add(listMethod);
			listMethod.setBoundaryBean(boundaryBean);
			panel.setBoundaryMethod(listMethod);
		}
		else
			panel.setBoundaryMethod(existingMethod);

		// Initialize the boundary methods for additional form actions
		for (final FormAction action : panel.getActions()) {
			if (action.getType() == ActionType.DELETE || action.getType() == ActionType.DOWNLOAD
					|| action.getType() == ActionType.COPY) {
				BoundaryBean boundary = null;
				final DomainObject panelDomainObj = panel.getDTO().getDomainObject();

				boundary = boundaryService.getOrCreateBoundaryOfDomainObject(panelDomainObj);

				addBoundaryMethodToAction(action, boundary, downloadMap);

				boundaryBuildList.add(boundary);
			}

			action.setPanel(panel);
		}

		formGroup.getPanels().add(panel);

		panel.getDTO().setNamespace(dtoNamespace);
		dtoNamespace.getJavaTypes().add(panel.getDTO());

		project.eResource().getContents().add(panel);

		if (panel.getFormTable() != null) {
			final FormTable t = panel.getFormTable();

			if (!project.eResource().getContents().contains(t))
				project.eResource().getContents().add(t);
		}

		// Save the DTO
		dtoBuildList.stream().filter(dto -> !project.eResource().getContents().contains(dto))
				.forEach(dto -> project.eResource().getContents().add(dto));

		// Save boundaries and repositories
		boundaryBuildList.forEach(b -> {
			if (!project.eResource().getContents().contains(b))
				project.eResource().getContents().add(b);

			if (!project.eResource().getContents().contains(b.getRepository()))
				project.eResource().getContents().add(b.getRepository());
		});

		// Synchronize integration beans
		integrationSyncService.sync();

		EclipseIDEService.saveProjectMetaData(project);

		// Build the DTO
		for (final DTOBean dto : dtoBuildList)
			dtoService.rebuildDTOBeanSourceFiles(dto);

		// Build the boundaries
		for (final BoundaryBean b : boundaryBuildList) {
			boundaryService.rebuildBoundarySourceFiles(b);

			if (project.isBoundaryMode())
				repositoryService.rebuildRepositorySourceFiles(b.getRepository());
		}

		// Build the grid panel
		rebuildGridPanel(panel);

		// Synchronize GUI tests
		guiTestCaseService.syncOnCreateGridPanel(panel);
	}

	/**
	 * Add new actions to the grid panel. Note that download and delete actions are not synchronized!
	 * @param panel
	 * @return the number of new actions
	 * @throws Exception if an internal error has occurred
	 */
	public int syncActions(FormPanel panel) throws Exception {
		final DomainObject domainObject = panel.getDTO().getDomainObject();
		final DomainObject refDomainObject = panel.getAssociation().getDomainObject();
		boolean addAllActions = true;
		int counter = 0;

		if (panel.getAssociation() instanceof ManyToManyAssociation
				|| panel.getAssociation() instanceof final OneToManyAssociation otm && !otm.isBidirectional())
			addAllActions = false;

		// Search for possible forms that can be used for new actions
		final var formsOfDomainObject = new BasicEList<Form>();

		for (final Form f : project.getAllFormsOfProject())
			if (f.getDomainObject().equals(domainObject)) {
				final FormTypeEnumeration formType = f.getFormType();

				if (addAllActions && (f.getFormType() == FormTypeEnumeration.CREATE || f.getFormType() == FormTypeEnumeration.UPDATE))
					formsOfDomainObject.add(f);

				// Check if the grid panel can provide the parent ID for a form of type 'ADD'!
				if (addAllActions && f.getFormType() == FormTypeEnumeration.ADD)
					for (final FormField field : f.getAllFormFields()) {
						if (field.getFieldType() != FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
							continue;

						if (field.getDTOAttribute().getReferencedDTOBean().getDomainObject().equals(refDomainObject))
							formsOfDomainObject.add(f);
					}

				if (formType == FormTypeEnumeration.READONLY)
					formsOfDomainObject.add(f);
			}

		for (final Form f : formsOfDomainObject) {
			final FormAction action = createAction(panel.getActions(), f);

			if (action == null)
				continue;

			action.setTargetForm(f);
			action.setPanel(panel);
			panel.getActions().add(action);
			counter++;
		}

		if (counter == 0)
			return counter;

		EclipseIDEService.saveProjectMetaData(project);

		// Rebuild the grid panel
		rebuildGridPanel(panel);

		return counter;
	}

	/**
	 * Add new actions to the given form. Note that download and delete actions are not synchronized!
	 * @param form
	 * @return the number of new actions
	 * @throws Exception if an internal error has occurred
	 */
	public int syncActions(Form form) throws Exception {
		final DomainObject domainObject = form.getDTO().getDomainObject();
		int counter = 0;

		// Search for possible forms that can be used for new actions
		final var formsOfDomainObject = new BasicEList<Form>();

		for (final Form f : project.getAllFormsOfProject())
			if (f.getDomainObject().equals(domainObject)) {
				final FormTypeEnumeration formType = f.getFormType();

				if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY
						|| formType == FormTypeEnumeration.CREATE)
					formsOfDomainObject.add(f);
			}

		for (final Form f : formsOfDomainObject) {
			final FormAction action = createAction(form.getActions(), f);

			if (action == null)
				continue;

			counter++;
			action.setTargetForm(f);
			action.setForm(form);

			form.getActions().add(action);
		}

		if (counter == 0)
			return counter;

		EclipseIDEService.saveProjectMetaData(project);

		// Rebuild the form
		rebuildForm(form);

		return counter;
	}

	/**
	 * @param form
	 * @return a list of boundary beans with new methods added
	 * @throws Exception if an internal error has occurred
	 */
	public EList<BoundaryBean> addAdditionalBoundaryMethods(Form form) throws Exception {
		final var boundaryBuildList = new BasicEList<BoundaryBean>();

		for (final FormField field : form.getAllFormFields()) {
			if (field.getDTOAttribute().getReferencedDTOBean() == null)
				continue;

			final DomainObject listDomainObject = field.getDTOAttribute().getReferencedDTOBean().getDomainObject();
			final BoundaryBean listBoundary = boundaryService.getOrCreateBoundaryOfDomainObject(listDomainObject);

			// Add a boundary method to search for the list DTO
			final BoundaryMethod listMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
			listMethod.setName("find" + listDomainObject.getNamePlural());
			listMethod.setComment("Search for " + listDomainObject.getLabel() + " objects");
			listMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
			listMethod.setReturnType(field.getDTOAttribute().getReferencedDTOBean());
			listMethod.setMethodType(BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
			listMethod.setQueryStatement("from " + listDomainObject.getName() + " a");

			if (field.getDTOAttribute().getAssociation() instanceof final OneToManyAssociation otm && !otm.isBidirectional()) {
				// Just return objects that are not referenced yet!
				final var filterStatement = new StringBuilder();
				filterStatement.append(" where a not in (select c from ");
				filterStatement.append(otm.getDomainObject().getName());
				filterStatement.append(" b join b.");
				filterStatement.append(otm.getName());
				filterStatement.append(" c)");

				listMethod.setQueryStatement(listMethod.getQueryStatement() + filterStatement.toString());
			}

			listMethod.setCustomStatement("");

			RepositoryMethod repositoryMethod = listBoundary.getRepository()
					.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

			listMethod.setServiceMethod(repositoryMethod);

			final MethodParameter paramList = JavaFactory.eINSTANCE.createMethodParameter();
			paramList.setName("filter");
			paramList.setType(project.getJavaTypeByName(JavaType.STRING));
			paramList.setMethod(listMethod);

			listMethod.getMethodParameters().add(paramList);

			BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(listMethod, listBoundary);

			if (existingMethod == null) {
				listMethod.setBoundaryBean(listBoundary);
				listBoundary.getBoundaryMethods().add(listMethod);
			}

			if (field.getDTOAttribute().getReferencedDTOBean().getAttributes().size() > 1) {
				// Add a boundary method to find the DTO by its primary key attribute
				final BoundaryMethod findIdMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
				findIdMethod.setName("findList" + field.getDTOAttribute().getReferencedDTOBean().getDomainObject().getName());
				findIdMethod
						.setComment("Find " + field.getDTOAttribute().getReferencedDTOBean().getDomainObject().getLabel() + " by its ID");
				findIdMethod.setReturnType(field.getDTOAttribute().getReferencedDTOBean());
				findIdMethod.setMethodType(BoundaryMethodTypeEnumeration.FIND_BY_ID);

				repositoryMethod = listBoundary.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

				findIdMethod.setServiceMethod(repositoryMethod);

				final DTOBeanAttribute pkAttr = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute();

				final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
				paramId.setName("id");
				paramId.setType(pkAttr.getDomainAttribute().getJavaType());
				paramId.setMethod(findIdMethod);

				findIdMethod.getMethodParameters().add(paramId);

				existingMethod = boundaryService.checkBoundaryMethod(findIdMethod, listBoundary);

				if (existingMethod == null) {
					findIdMethod.setBoundaryBean(listBoundary);
					listBoundary.getBoundaryMethods().add(findIdMethod);
				}
			}

			boundaryBuildList.add(listBoundary);
		}

		return boundaryBuildList;
	}

	/**
	 * Create a new single-record form
	 * @param form
	 * @param listDTOMap
	 * @param roles
	 * @param formGroup
	 * @param namespaceMap
	 * @param addDTOMap
	 * @param boundaryMethodName
	 * @param returnVoid
	 * @throws Exception if an internal error has occurred
	 */
	public void createUpdateForm(Form form, Map<String, DTOBean> listDTOMap, Collection<Role> roles, FormGroup formGroup,
			Map<DTOBean, Namespace> namespaceMap, Map<String, DTOBean> addDTOMap, String boundaryMethodName, boolean returnVoid)
			throws Exception {
		final var dtoBuildList = new BasicEList<DTOBean>();
		final var boundaryBuildList = new BasicEList<BoundaryBean>();
		final DomainObject domainObject = form.getDomainObject();
		final var dtoSet = new HashSet<DTOBean>();
		final DTOBean formDTO = form.getDTO();
		final Namespace formDTONamespace = namespaceMap.get(form.getDTO());

		// Omit the test for shared data transfer objects that already exist!
		if (formDTONamespace != null)
			dtoSet.add(form.getDTO());

		// Search for additional data transfer objects of initial one-to-many associations that must be checked
		if (form.getFormType() == FormTypeEnumeration.ADD || form.getFormType() == FormTypeEnumeration.CREATE)
			for (final FormPanel panel : form.getFormPanels()) {
				if (panel.getBasePanel() != null || panel.getDTO() == null)
					continue;

				dtoSet.add(panel.getDTO());
			}

		if (project.isBoundaryMode()) {
			// Validate all data transfer objects
			for (final DTOBean dto : dtoSet) {
				final IStatus status = mappingObjectService.validateMappingObject(dto);

				if (!status.isOK())
					throw new IllegalStateException(status.getMessage());
			}
		}
		else {
			for (final DTOBean dto : dtoSet) {
				int counter = 0;
				boolean isNameValid = false;
				String name = dto.getName();

				// Although the DTO name doesn't matter in this case we want to have a unique one!
				while (!isNameValid) {
					isNameValid = true;

					for (final DTOBean existingDTO : project.getAllDTOsOfProject()) {
						if (existingDTO.equals(dto))
							continue;

						final String existingPackage = existingDTO.getNamespace().getName();
						final String packageName = dto.getDomainObject().getNamespace().getName();

						if (existingDTO.getName().equals(name) && existingPackage.equals(packageName)) {
							name = dto.getName() + ++counter;
							isNameValid = false;
							break;
						}
					}
				}

				dto.setName(name);
			}
		}

		// Check if a form with same name already exists
		for (final Form f : project.getAllFormsOfProject()) {
			if (f.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW || f.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
				continue;

			if (f.getFormType() == FormTypeEnumeration.TREE_VIEW)
				continue;

			if (f.getFormType() == FormTypeEnumeration.LOV || f.getFormType() == FormTypeEnumeration.GRID)
				continue;

			if (form.equals(f))
				continue;

			if (form.getName().equals(f.getName()))
				throw new IllegalStateException("A form with this name already exists!");
		}

		dtoBuildList.add(formDTO);

		listDTOMap.values().forEach(dtoBuildList::add);

		// Grant all selected roles
		form.getRoles().addAll(roles);

		final BoundaryBean boundaryBean = boundaryService.getOrCreateBoundaryOfDomainObject(domainObject);

		// Create the boundary method
		BoundaryMethod boundaryMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();

		if (form.getFormType() == FormTypeEnumeration.CREATE || form.getFormType() == FormTypeEnumeration.ADD) {
			boundaryMethod.setName(boundaryMethodName);
			boundaryMethod.setComment("Create new " + form.getDomainObject().getLabel());

			if (!returnVoid)
				boundaryMethod.setReturnType(formDTO);
			else
				boundaryMethod.setReturnType(project.getJavaTypeByName(JavaType.VOID));

			boundaryMethod.setMethodType(BoundaryMethodTypeEnumeration.CREATE);

			final RepositoryMethod repositoryMethod = boundaryBean.getRepository()
					.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST);

			boundaryMethod.setServiceMethod(repositoryMethod);

			final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setName("object");
			param.setType(formDTO);
			param.setMethod(boundaryMethod);
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			boundaryMethod.getMethodParameters().add(param);

			// Add further parameters for all initial one-to-many associations
			for (final Map.Entry<String, DTOBean> entry : addDTOMap.entrySet()) {
				final MethodParameter addParam = JavaFactory.eINSTANCE.createMethodParameter();
				addParam.setName(entry.getKey());
				addParam.setType(entry.getValue());
				addParam.setMethod(boundaryMethod);
				addParam.setModifier(JavaTypeModifierEnumeration.NONE);

				boundaryMethod.getMethodParameters().add(addParam);
				dtoBuildList.add(entry.getValue());
			}

			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(boundaryMethod, boundaryBean);

			if (existingMethod == null) {
				boundaryMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(boundaryMethod);
			}
			else
				boundaryMethod = existingMethod;

		}
		else if (form.getFormType() == FormTypeEnumeration.UPDATE) {
			boundaryMethod.setName(boundaryMethodName);
			boundaryMethod.setComment("Update existing " + form.getDomainObject().getLabel() + " object");

			if (!returnVoid)
				boundaryMethod.setReturnType(formDTO);
			else
				boundaryMethod.setReturnType(project.getJavaTypeByName(JavaType.VOID));

			boundaryMethod.setMethodType(BoundaryMethodTypeEnumeration.UPDATE);

			final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setName("object");
			param.setType(formDTO);
			param.setMethod(boundaryMethod);
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			boundaryMethod.getMethodParameters().add(param);

			final RepositoryMethod repositoryMethod = boundaryBean.getRepository()
					.getMethodByType(RepositoryMethodTypeEnumeration.MERGE);

			boundaryMethod.setServiceMethod(repositoryMethod);

			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(boundaryMethod, boundaryBean);

			if (existingMethod == null) {
				boundaryMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(boundaryMethod);
			}
			else
				boundaryMethod = existingMethod;

			final BoundaryMethod finderMethod = createFinderMethod(form, boundaryBean, null);

			if (boundaryService.checkBoundaryMethod(finderMethod, boundaryBean) == null) {
				finderMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(finderMethod);
			}
		}
		else if (form.getFormType() == FormTypeEnumeration.READONLY) {
			boundaryMethod = createFinderMethod(form, boundaryBean, boundaryMethodName);

			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(boundaryMethod, boundaryBean);

			if (existingMethod == null) {
				boundaryMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(boundaryMethod);
			}
			else
				boundaryMethod = existingMethod;
		}

		boundaryBuildList.add(boundaryBean);

		// Create additional boundary methods for many-to-many and many-to-one relationships
		boundaryBuildList.addAll(addAdditionalBoundaryMethods(form));

		final FormAction action = ClientFactory.eINSTANCE.createFormAction();

		if (form.getFormType() == FormTypeEnumeration.CREATE || form.getFormType() == FormTypeEnumeration.ADD) {
			action.setName(ACTION_CREATE);
			action.setType(ActionType.CREATE);
		}
		else if (form.getFormType() == FormTypeEnumeration.UPDATE) {
			action.setName(ACTION_UPDATE);
			action.setType(ActionType.UPDATE);
		}
		else if (form.getFormType() == FormTypeEnumeration.READONLY) {
			action.setName(ACTION_GET);
			action.setType(ActionType.READ);
		}

		action.setForm(form);
		action.setBoundaryMethod(boundaryMethod);

		form.getActions().add(action);
		form.setFormGroup(formGroup);

		formGroup.getForms().add(form);

		createAdditionalLobActions(form, boundaryBean);

		namespaceMap.keySet().forEach(b -> {
			final Namespace n = namespaceMap.get(b);
			b.setNamespace(n);
			n.getJavaTypes().add(b);
		});

		project.eResource().getContents().add(form);

		// Save the form panels
		for (final FormPanel p : form.getFormPanels())
			if (!project.eResource().getContents().contains(p))
				project.eResource().getContents().add(p);

		// Save the DTO
		for (final DTOBean dto : dtoBuildList)
			if (!project.eResource().getContents().contains(dto))
				project.eResource().getContents().add(dto);

		// Save boundaries and repositories
		boundaryBuildList.forEach(b -> {
			if (!project.eResource().getContents().contains(b))
				project.eResource().getContents().add(b);

			if (!project.eResource().getContents().contains(b.getRepository()))
				project.eResource().getContents().add(b.getRepository());
		});

		// Synchronize integration beans
		integrationSyncService.sync();

		EclipseIDEService.saveProjectMetaData(project);

		// Save and build the DTO
		for (final DTOBean dto : dtoBuildList)
			dtoService.rebuildDTOBeanSourceFiles(dto);

		// Save and build the boundaries
		for (final BoundaryBean b : boundaryBuildList) {
			boundaryService.rebuildBoundarySourceFiles(b);

			if (project.isBoundaryMode())
				repositoryService.rebuildRepositorySourceFiles(b.getRepository());
		}

		// Build the form
		rebuildForm(form);

		// Synchronize GUI tests
		guiTestCaseService.syncOnCreateForm(form);
	}

	/**
	 * Rename a given form
	 * @param form
	 * @param newName
	 * @throws IllegalStateException if the new name is invalid
	 * @throws Exception if an internal error has occurred
	 */
	public void renameForm(Form form, String newName) throws Exception {
		if (form.getName().equals(newName))
			return;

		final String oldName = form.getName();

		// Validate the name
		final IStatus status = EclipseIDEService.validateJavaTypeName(newName);

		if (status.getSeverity() > IStatus.INFO)
			throw new IllegalStateException(status.getMessage());

		final WorkspaceFile sourceFile = form.getTypeScriptSourceFile();

		if (sourceFile != null)
			EclipseIDEService.deleteWorkspaceFile(sourceFile);
		else
			EclipseIDEService.renameCompUnit(form.getSourceFile(), newName);

		// Delete the file that contains the visual representation of the form
		final WorkspaceFile uiFile = form.getUserInterfaceFile();

		if (uiFile != null)
			EclipseIDEService.deleteWorkspaceFile(uiFile);

		// Save the new name
		form.setName(newName);

		// Rebuild the form
		rebuildForm(form);

		// Synchronize GUI tests
		new GUITestCaseService(project).syncOnRenameForm(form, oldName);
	}

	/**
	 * Rename a given grid panel
	 * @param gridPanel
	 * @param newName
	 * @throws IllegalStateException if the new name is invalid
	 * @throws Exception if an internal error has occurred
	 */
	public void renameGridPanel(FormPanel gridPanel, String newName) throws Exception {
		if (gridPanel.getName().equals(newName))
			return;

		final String oldName = gridPanel.getName();

		// Validate the name
		final IStatus status = EclipseIDEService.validateJavaTypeName(newName);

		if (status.getSeverity() > IStatus.INFO)
			throw new IllegalStateException(status.getMessage());

		final WorkspaceFile sourceFile = gridPanel.getTypeScriptSourceFile();

		if (sourceFile != null)
			EclipseIDEService.deleteWorkspaceFile(sourceFile);
		else
			EclipseIDEService.renameCompUnit(gridPanel.getSourceFile(), newName);

		// Delete the file that contains the visual representation of the grid panel
		final WorkspaceFile uiFile = gridPanel.getUserInterfaceFile();

		if (uiFile != null)
			EclipseIDEService.deleteWorkspaceFile(uiFile);

		// Save the new name
		gridPanel.setName(newName);

		// Rebuild the grid panel
		rebuildGridPanel(gridPanel);

		// Synchronize GUI tests
		new GUITestCaseService(project).syncOnRenameGridPanel(gridPanel, oldName);
	}

	/**
	 * @param form
	 * @throws Exception if either the form or one of the respective source files could not be deleted
	 */
	public void deleteForm(Form form) throws Exception {
		final String projectName = project.getTargetProjectName(BuildArtifactType.GUI);
		final FormTypeEnumeration formType = form.getFormType();
		final BoundaryBean boundary = project.getBoundaryByDomainObject(form.getDomainObject());
		final WorkspaceFile uiFile = form.getUserInterfaceFile();
		final var methodSet = new HashSet<BoundaryMethod>();
		final var dtoSet = new HashSet<DTOBean>();
		String packageName = project.getClientNamespace().toString();

		checkDeleteForm(form);

		if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW)
			packageName += PACK_CLIENT_VIEW;
		else if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD
				|| formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			packageName += PACK_CLIENT_DLG;
		else if (formType == FormTypeEnumeration.LOV)
			packageName += PACK_CLIENT_LOV;
		else if (formType == FormTypeEnumeration.TREE_VIEW)
			packageName += PACK_CLIENT_TREE;

		if (project.hasEclipseClient() && (formType == FormTypeEnumeration.SIMPLE_VIEW
				|| formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.TREE_VIEW)) {
			// Remove the view from the application model
			EclipseIDEService.removeViewFromApplicationModel(projectName, packageName + "." + form.getName());
		}

		// Delete the file that contains the visual representation of the form
		if (uiFile != null)
			EclipseIDEService.deleteWorkspaceFile(uiFile);

		// Delete the source file
		final WorkspaceFile sourceFile = form.getTypeScriptSourceFile();

		if (sourceFile != null)
			EclipseIDEService.deleteWorkspaceFile(sourceFile);
		else
			EclipseIDEService.deleteSource(form.getSourceFile());

		if (project.hasAngularClient())
			new AngularFormModuleGenerator(form.getDomainObject()).createSourceFile();

		// Synchronize GUI tests
		guiTestCaseService.syncOnDeleteForm(form);

		if (isDTODeleteAllowed(form.getDTO(), form, boundary))
			dtoSet.add(form.getDTO());

		// Search for additional data transfer objects of initial one-to-many associations that must be deleted also!
		if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE)
			for (final FormPanel panel : form.getFormPanels()) {
				if (panel.getBasePanel() != null)
					continue;

				if (panel.getDTO() != null && isDTODeleteAllowed(panel.getDTO(), form, boundary))
					dtoSet.add(panel.getDTO());
			}

		for (final DTOBean dto : dtoSet) {
			// Delete all boundary methods that use this DTO!
			for (final BoundaryMethod method : boundary.getBoundaryMethods()) {
				if (method.isUsedByIntegrationMethod())
					continue;

				if (method.getReturnType().equals(dto))
					methodSet.add(method);

				for (final MethodParameter p : method.getMethodParameters())
					if (p.getType().equals(dto))
						methodSet.add(method);
			}

			methodSet.forEach(m -> {
				boundary.getBoundaryMethods().remove(m);
				project.eResource().getContents().remove(m);
			});

			// Delete the corresponding data transfer object
			EclipseIDEService.deleteSource(dto.getSourceFile());

			project.eResource().getContents().remove(dto);

			if (formType == FormTypeEnumeration.SEARCHABLE_VIEW) {
				// Delete the respective count method
				for (final FormPanel p : form.getFormPanels())
					if (p.getBoundaryMethod() != null) {
						if (p.getBoundaryMethod().isUsedByIntegrationMethod())
							continue;

						boundary.getBoundaryMethods().remove(p.getBoundaryMethod());
						project.eResource().getContents().remove(p.getBoundaryMethod());
						break;
					}
			}
		}

		if (formType == FormTypeEnumeration.TREE_VIEW) {
			final var tree = (TreeView) form;

			// Only delete the count method. If a tree view uses a recursive method, it has already been removed in previous steps!
			if (tree.getCountMethod() != null && !tree.getCountMethod().isUsedByIntegrationMethod()) {
				boundary.getBoundaryMethods().remove(tree.getCountMethod());
				project.eResource().getContents().remove(tree.getCountMethod());
			}

			// Remove all boundary methods and data transfer objects of all sub-items!
			for (final TreeViewItem treeViewItem : tree.getAllSubTreeItems()) {
				if (!treeViewItem.getDataFetchMethod().isUsedByIntegrationMethod()) {
					final BoundaryBean b = treeViewItem.getDataFetchMethod().getBoundaryBean();
					b.getBoundaryMethods().remove(treeViewItem.getDataFetchMethod());

					project.eResource().getContents().remove(treeViewItem.getDataFetchMethod());

					boundaryService.rebuildBoundarySourceFiles(b);
				}

				final DTOBean itemDTO = treeViewItem.getItemDTO();

				if (!itemDTO.isUsedByIntegrationMethod()) {
					EclipseIDEService.deleteSource(itemDTO.getSourceFile());

					project.eResource().getContents().remove(itemDTO);
				}
			}
		}

		// Delete the list DTOs of all fields. If a list DTO is not mapped to a form field it won't be deleted automatically!
		for (final FormField field : form.getAllFormFields()) {
			final DTOBean referencedDTO = field.getDTOAttribute().getReferencedDTOBean();

			// Do not try to delete the same DTO multiple times by checking if its namespace is null!
			if (referencedDTO == null || referencedDTO.getNamespace() == null)
				continue;

			deleteListDTO(referencedDTO, null, form);
		}

		// Delete the panels of the form
		form.getFormPanels().forEach(p -> {
			if (p.getFormTable() != null)
				form.eResource().getContents().remove(p.getFormTable());

			form.eResource().getContents().remove(p);
		});

		final FormGroup group = form.getFormGroup();
		group.getForms().remove(form);

		form.eResource().getContents().remove(form);

		EclipseIDEService.saveProjectMetaData(project);

		// Rebuild the application tree navigator
		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW
				|| formType == FormTypeEnumeration.TREE_VIEW)
			rebuildNavigator();

		boundaryService.rebuildBoundarySourceFiles(boundary);
	}

	/**
	 * Delete the given grid panel
	 * @param panel
	 * @throws IllegalStateException if the grid panel is referenced by an existing form
	 * @throws Exception if either the grid panel or the respective source file could not be deleted
	 */
	public void deleteGridPanel(FormPanel panel) throws Exception {
		final BoundaryBean boundary = panel.getBoundaryMethod().getBoundaryBean();

		// Test if the panel is used in a form
		for (final Form form : project.getAllFormsOfProject())
			for (final FormPanel p : form.getFormPanels())
				if (p.getBasePanel() != null && p.getBasePanel().equals(panel))
					throw new IllegalStateException("The grid panel is referenced by the form '" + form.getName() + "'!");

		// Test if the grid panel's DTO can be safely deleted!
		final boolean deleteDTO = isDTODeleteAllowed(panel.getDTO(), null, boundary);
		final WorkspaceFile uiFile = panel.getUserInterfaceFile();

		// Delete the file that contains the visual representation of the grid panel
		if (uiFile != null)
			EclipseIDEService.deleteWorkspaceFile(uiFile);

		// Delete the source file
		final WorkspaceFile sourceFile = panel.getTypeScriptSourceFile();

		if (sourceFile != null)
			EclipseIDEService.deleteWorkspaceFile(sourceFile);
		else
			EclipseIDEService.deleteSource(panel.getSourceFile());

		if (project.hasAngularClient())
			new AngularFormModuleGenerator(panel.getDTO().getDomainObject());

		// Synchronize GUI tests
		guiTestCaseService.syncOnDeleteGridPanel(panel);

		if (deleteDTO) {
			dtoService.removeDTOBeanSourceFile(panel.getDTO());

			project.eResource().getContents().remove(panel.getDTO());
		}

		if (!panel.getBoundaryMethod().isUsedByIntegrationMethod()) {
			boundary.getBoundaryMethods().remove(panel.getBoundaryMethod());
			project.eResource().getContents().remove(panel.getBoundaryMethod());
		}

		// Remove the form table from the meta-model
		project.eResource().getContents().remove(panel.getFormTable());

		// Remove the panel from the meta-model
		project.eResource().getContents().remove(panel);

		EclipseIDEService.saveProjectMetaData(project);

		boundaryService.rebuildBoundarySourceFiles(boundary);
	}

	/**
	 * Delete the given form field
	 * @param formField
	 * @throws IllegalStateException if the field is mapped to the primary key attribute of the corresponding domain object
	 * @throws Exception if an internal error has occurred
	 */
	public void deleteFormField(FormField formField) throws Exception {
		final DTOBean dto = formField.getDTOAttribute().getDTOBean();
		final FormPanel panel = formField.getPanel();
		final Form form = panel.getForm();
		final DTOBean listDTO = formField.getDTOAttribute().getReferencedDTOBean();
		boolean removeDTOAttribute = true;

		// It is not allowed to delete the primary key attribute of the DTO!
		if (dto.getPKAttribute().equals(formField.getDTOAttribute()))
			throw new IllegalStateException("The ID attribute must not be deleted!");

		// If the DTO is shared we must check if other forms contain fields that reference the given attribute!
		if (dto.isShared())
			for (final Form f : project.getAllFormsOfProject()) {
				if (f.equals(form) || !f.getDTO().equals(dto))
					continue;

				for (final FormField field : f.getAllFormFields())
					if (formField.getDTOAttribute().equals(field.getDTOAttribute())) {
						removeDTOAttribute = false;
						break;
					}
			}

		// Synchronize GUI tests
		guiTestCaseService.syncOnDeleteField(formField);

		if (removeDTOAttribute) {
			dto.getAttributes().remove(formField.getDTOAttribute());

			// If the field references a list DTO we can only try to delete it if it's already bound to a namespace!
			if (listDTO != null && listDTO.getNamespace() != null)
				deleteListDTO(formField.getDTOAttribute().getReferencedDTOBean(), formField, null);
		}

		int rowIndex = -1;

		if (panel.getColumnCount() == 1)
			rowIndex = formField.getRowIndex();

		panel.getFields().remove(formField);

		// Change the row indexes of the following fields
		if (rowIndex != -1)
			for (final FormField f : panel.getFields())
				if (f.getRowIndex() > rowIndex)
					f.setRowIndex(f.getRowIndex() - 1);
	}

	/**
	 * @param listDTO
	 * @param fieldToDelete
	 * @param formToDelete
	 * @throws Exception if either the DTO or the respective source file could not be deleted
	 */
	public void deleteListDTO(DTOBean listDTO, FormField fieldToDelete, Form formToDelete) throws Exception {
		final BoundaryBean boundary = project.getBoundaryByDomainObject(listDTO.getDomainObject());

		// The data transfer object must not be deleted if it is used by an integration method!
		if (listDTO.isUsedByIntegrationMethod())
			return;

		// Test if the list DTO is used in other forms
		for (final Form form : project.getAllFormsOfProject()) {
			if (formToDelete != null) {
				// If both forms are equal further tests can be skipped!
				if (formToDelete.equals(form))
					continue;

				// If two forms share the same DTO we don't delete a list DTO!
				if (formToDelete.getDTO().isShared() && form.getDTO().equals(formToDelete.getDTO()))
					return;
			}

			if (fieldToDelete != null) {
				final Form formOfField = fieldToDelete.getPanel().getForm();

				// If two forms share the same DTO a list DTO won't be deleted!
				if (formOfField.getDTO().isShared() && !formOfField.equals(form) && form.getDTO().equals(formOfField.getDTO()))
					return;
			}

			for (final FormField field : form.getAllFormFields()) {
				// Check if the test for the current field can be skipped
				if (fieldToDelete != null && fieldToDelete.equals(field))
					continue;

				if (field.getDTOAttribute().getReferencedDTOBean() == null)
					continue;

				if (field.getDTOAttribute().getReferencedDTOBean().equals(listDTO))
					return;
			}
		}

		// Test if an attribute exists that references the given list DTO!
		for (final DTOBean b : project.getAllDTOsOfProject()) {
			if (b.equals(listDTO))
				continue;

			// If we delete a form we don't have to check the form's DTO!
			if (formToDelete != null && formToDelete.getDTO().equals(b))
				continue;

			for (final DTOBeanAttribute a : b.getAttributes()) {
				// If we delete a form field we don't have to check the field's DTO attribute!
				if (fieldToDelete != null && fieldToDelete.getDTOAttribute().equals(a))
					continue;

				if (a.getReferencedDTOBean() != null && a.getReferencedDTOBean().equals(listDTO))
					return;
			}
		}

		final var methodSet = new HashSet<BoundaryMethod>();

		// Search for respective boundary method(s) to be removed also!
		if (boundary != null) {
			for (final BoundaryMethod m : boundary.getBoundaryMethods()) {
				if (!m.getReturnType().equals(listDTO))
					continue;

				if (!m.isUsedByIntegrationMethod() && (m.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER
						|| m.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_ID))
					methodSet.add(m);
			}

			methodSet.forEach(m -> {
				boundary.getBoundaryMethods().remove(m);
				project.eResource().getContents().remove(m);
			});

			boundaryService.rebuildBoundarySourceFiles(boundary);
		}

		if (project.isBoundaryMode())
			dtoService.removeDTOBeanSourceFile(listDTO);

		listDTO.getNamespace().getJavaTypes().remove(listDTO);
		project.eResource().getContents().remove(listDTO);
	}

	/**
	 * @param formDTO the data transfer object to be tested
	 * @param currentForm the form the data transfer belongs to or null if the DTO belongs to a grid panel
	 * @param boundary the boundary bean of the form or grid panel
	 * @return true if the data transfer object can be deleted
	 */
	private boolean isDTODeleteAllowed(DTOBean formDTO, Form currentForm, BoundaryBean boundary) {
		if (formDTO.isUsedByIntegrationMethod())
			return false;

		// Check if the DTO is referenced by other boundary methods in other boundary beans
		for (final BoundaryBean b : project.getAllBoundariesOfProject()) {
			// We must not check this boundary!
			if (b.equals(boundary))
				continue;

			for (final BoundaryMethod m : b.getBoundaryMethods()) {
				if (m.getReturnType().equals(formDTO))
					return false;

				for (final MethodParameter p : m.getMethodParameters())
					if (p.getType().equals(formDTO))
						return false;
			}
		}

		for (final DTOBean b : project.getAllDTOsOfProject()) {
			if (b.equals(formDTO))
				if (b.isShared()) {
					// Test if other forms share the same data transfer object
					if (currentForm != null && b.equals(formDTO))
						for (final Form form : project.getAllFormsOfProject())
							if (!form.equals(currentForm) && form.getDTO().equals(formDTO))
								return false;
				}
				else
					continue;

			// Test if an attribute exists that references the given form DTO!
			for (final DTOBeanAttribute a : b.getAttributes())
				if (a.getReferencedDTOBean() != null && a.getReferencedDTOBean().equals(formDTO))
					return false;
		}

		return true;
	}

	/**
	 * Create a finder method for forms of type 'UPDATE' and 'READONLY'
	 * @param form
	 * @param boundaryBean
	 * @param defaultName
	 * @return the boundary method
	 */
	private BoundaryMethod createFinderMethod(final Form form, final BoundaryBean boundaryBean, final String defaultName) {
		final DTOBean formDTO = form.getDTO();
		final DomainObject domainObject = formDTO.getDomainObject();
		final DomainAttribute pkAttr = domainObject.getPKAttribute();
		final String methodName;

		if (defaultName == null)
			if (form.getFormType() == FormTypeEnumeration.UPDATE && !form.getDTO().isShared())
				methodName = "get" + domainObject.getName() + "ForUpdate";
			else
				methodName = "find" + domainObject.getName() + "ById";
		else
			methodName = defaultName;

		final BoundaryMethod finderMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		finderMethod.setName(methodName);
		finderMethod.setComment("Find " + domainObject.getLabel() + " by its ID");
		finderMethod.setReturnType(formDTO);
		finderMethod.setMethodType(BoundaryMethodTypeEnumeration.FIND_BY_ID);
		finderMethod.setServiceMethod(boundaryBean.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID));

		final MethodParameter paramGetter = JavaFactory.eINSTANCE.createMethodParameter();
		paramGetter.setName(pkAttr.getName());
		paramGetter.setType(pkAttr.getJavaType());
		paramGetter.setMethod(finderMethod);
		paramGetter.setModifier(JavaTypeModifierEnumeration.NONE);

		finderMethod.getMethodParameters().add(paramGetter);

		return finderMethod;
	}

	/**
	 * Check if the form can be deleted
	 * @param form
	 * @throws IllegalStateException if the form is referenced by other objects of the internal meta-model
	 */
	private void checkDeleteForm(Form form) {
		final FormTypeEnumeration formType = form.getFormType();

		// Test if the form is referenced by other forms
		if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.UPDATE
				|| formType == FormTypeEnumeration.READONLY) {
			for (final Form f : project.getAllFormsOfProject()) {
				if (f.equals(form))
					continue;

				for (final FormAction a : f.getActions())
					if (a.getTargetForm() != null && a.getTargetForm().equals(form))
						throw new IllegalStateException("The form is referenced by the form '" + f.getName() + "'!");
			}

			for (final FormPanel p : project.getAllGridPanelsOfProject())
				for (final FormAction a : p.getActions())
					if (a.getTargetForm() != null && a.getTargetForm().equals(form))
						throw new IllegalStateException("The form is referenced by the grid panel '" + p.getName() + "'!");
		}

		// Test if the list-of-values can be deleted!
		if (formType == FormTypeEnumeration.LOV)
			for (final Form f : project.getAllFormsOfProject())
				for (final FormPanel p : f.getFormPanels()) {
					if (f.equals(form))
						continue;

					if (p.getBasePanel() != null)
						continue;

					if (p.getFormTable() == null) {
						for (final FormField field : p.getFields())
							if (field.getFieldType() == FormFieldTypeEnumeration.LOV && field.getListOfValues() != null
									&& field.getListOfValues().equals(form))
								throw new IllegalStateException(
										"The form is referenced by the field '" + field.getName() + "' of form '" + f.getName() + "'!");
					}
					else {
						for (final TableColumnField c : p.getFormTable().getFields())
							if (c.getLovForm() != null && c.getLovForm().equals(form))
								throw new IllegalStateException(
										"The form is referenced by the table column '" + c.getTitle() + "' of form '" + f.getName() + "'!");
					}
				}
	}

	/**
	 * Create a new form action
	 * @param actions
	 * @param targetForm
	 * @return a new form action. The method will return null if the action already exists!
	 */
	private FormAction createAction(EList<FormAction> actions, Form targetForm) {
		// Check if the action already exists!
		for (final FormAction a : actions)
			if (a.getTargetForm() != null && a.getTargetForm().equals(targetForm))
				return null;

		final FormAction action = ClientFactory.eINSTANCE.createFormAction();

		// Add the default roles to the action!
		action.getRoles().addAll(targetForm.getRoles());

		if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
			action.setDescription("Create new " + targetForm.getDomainObject().getLabel());
			action.setName(ACTION_CREATE);
			action.setType(ActionType.CREATE);
		}
		else if (targetForm.getFormType() == FormTypeEnumeration.ADD) {
			action.setDescription("Add new " + targetForm.getDomainObject().getLabel());
			action.setName(ACTION_ADD);
			action.setType(ActionType.CREATE);
		}
		else if (targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
			action.setDescription("Edit " + targetForm.getDomainObject().getLabel());
			action.setName(ACTION_UPDATE);
			action.setType(ActionType.UPDATE);
		}
		else if (targetForm.getFormType() == FormTypeEnumeration.READONLY) {
			action.setDescription("View " + targetForm.getDomainObject().getLabel());
			action.setName(ACTION_GET);
			action.setType(ActionType.READ);
		}

		return action;
	}

	/**
	 * Add a boundary method to the given action
	 * @param a
	 * @param boundaryBean
	 * @param downloadMap
	 */
	private void addBoundaryMethodToAction(FormAction a, BoundaryBean boundaryBean, Map<String, DomainAttribute> downloadMap) {
		final DomainObject domainObject = boundaryBean.getDomainObject();
		final Repository repository = boundaryBean.getRepository();

		if (a.getType() == ActionType.DELETE) {
			BoundaryMethod deleteMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
			deleteMethod.setComment("Delete existing " + domainObject.getLabel());
			deleteMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			deleteMethod.setMethodType(BoundaryMethodTypeEnumeration.DELETE);

			final RepositoryMethod repositoryMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.DELETE);

			deleteMethod.setServiceMethod(repositoryMethod);
			deleteMethod.setName(METHOD_PREFIX_DELETE + domainObject.getName());
			deleteMethod.setReturnType(repositoryMethod.getReturnType());

			for (final MethodParameter param : repositoryMethod.getMethodParameters()) {
				final MethodParameter p = JavaFactory.eINSTANCE.createMethodParameter();
				p.setName(param.getName());
				p.setMethod(deleteMethod);
				p.setType(param.getType());
				p.setModifier(param.getModifier());

				deleteMethod.getMethodParameters().add(p);
			}

			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(deleteMethod, boundaryBean);

			if (existingMethod == null) {
				deleteMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(deleteMethod);
			}
			else
				deleteMethod = existingMethod;

			a.setBoundaryMethod(deleteMethod);
		}
		else if (a.getType() == ActionType.COPY) {
			BoundaryMethod copyMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
			copyMethod.setComment("Create copy of selected " + domainObject.getLabel());
			copyMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			copyMethod.setMethodType(BoundaryMethodTypeEnumeration.COPY);

			final RepositoryMethod repositoryMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.COPY);

			copyMethod.setServiceMethod(repositoryMethod);
			copyMethod.setName(repositoryMethod.getName());

			final var returnObject = (DomainObject) repositoryMethod.getReturnType();
			copyMethod.setReturnType(returnObject.getPKAttribute().getJavaType());

			// The respective repository has two parameters. The boundary method only needs one!
			final MethodParameter param = repositoryMethod.getFirstParameter();

			final MethodParameter p = JavaFactory.eINSTANCE.createMethodParameter();
			p.setName(param.getName() + "Id");
			p.setMethod(copyMethod);

			final var paramObj = (DomainObject) repositoryMethod.getReturnType();
			p.setType(paramObj.getPKAttribute().getJavaType());
			p.setModifier(param.getModifier());

			copyMethod.getMethodParameters().add(p);

			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(copyMethod, boundaryBean);

			if (existingMethod == null) {
				copyMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(copyMethod);
			}
			else
				copyMethod = existingMethod;

			a.setBoundaryMethod(copyMethod);
		}
		else if (a.getType() == ActionType.DOWNLOAD) {
			final DomainAttribute attr = downloadMap.get(a.getName());
			final String methodName;

			if (attr.getDomainObject().equals(boundaryBean.getDomainObject()))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final RepositoryMethod repositoryMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

			BoundaryMethod downloadMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
			downloadMethod.setComment("Download binary data that is stored in attribute \"" + attr.getName() + "\"");
			downloadMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			downloadMethod.setMethodType(BoundaryMethodTypeEnumeration.DOWNLOAD);
			downloadMethod.setReturnType(domainObject.getNamespace().getProject().getJavaTypeByName(JavaType.STRING));
			downloadMethod.setDomainAttribute(attr);
			downloadMethod.setName(methodName);
			downloadMethod.setServiceMethod(repositoryMethod);

			for (final MethodParameter param : repositoryMethod.getMethodParameters()) {
				final MethodParameter p = JavaFactory.eINSTANCE.createMethodParameter();
				p.setName(param.getName());
				p.setMethod(downloadMethod);
				p.setType(param.getType());
				p.setModifier(param.getModifier());

				downloadMethod.getMethodParameters().add(p);
			}

			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(downloadMethod, boundaryBean);

			if (existingMethod == null) {
				downloadMethod.setBoundaryBean(boundaryBean);
				boundaryBean.getBoundaryMethods().add(downloadMethod);
			}
			else
				downloadMethod = existingMethod;

			a.setBoundaryMethod(downloadMethod);
		}
	}

	/**
	 * Add additional actions and boundary methods for LOB attributes
	 * @param form
	 * @param boundary
	 */
	private void createAdditionalLobActions(Form form, BoundaryBean boundary) {
		form.getDomainObject().getAllLobAttributes().forEach(attr -> {
			FormAction action = null;

			if ((form.getFormType() == FormTypeEnumeration.CREATE || form.getFormType() == FormTypeEnumeration.ADD)
					&& attr.isInsertable()) {
				final String actionName;

				if (attr.getDomainObject().equals(form.getDomainObject()))
					actionName = "upload" + attr.getUpperCaseName() + "Action";
				else
					actionName = "upload" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName() + "Action";

				// An action is necessary in order to set binary data
				action = ClientFactory.eINSTANCE.createFormAction();

				// Add the default roles to the action!
				action.getRoles().addAll(form.getRoles());

				action.setDescription("Browse file in order to upload " + attr.getLabel());
				action.setName(actionName);
				action.setType(ActionType.DIRECT_UPLOAD);
				action.setForm(form);

				form.getActions().add(action);
			}

			if (form.getFormType() == FormTypeEnumeration.UPDATE && attr.isUpdatable()) {
				final String actionName;

				if (attr.getDomainObject().equals(form.getDomainObject()))
					actionName = "upload" + attr.getUpperCaseName() + "Action";
				else
					actionName = "upload" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName() + "Action";

				// An action is necessary in order to set binary data
				action = ClientFactory.eINSTANCE.createFormAction();

				// Add the default roles to the action!
				action.getRoles().addAll(form.getRoles());

				action.setDescription("Browse file in order to upload " + attr.getLabel());
				action.setName(actionName);
				action.setType(ActionType.INDIRECT_UPLOAD);
				action.setForm(form);

				form.getActions().add(action);
			}

			if (action != null) {
				final String methodName;

				if (attr.getDomainObject().equals(form.getDomainObject()))
					methodName = "upload" + attr.getUpperCaseName();
				else
					methodName = "upload" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

				// We have to add an upload boundary method for both forms (insert and update). Although the insert form does not call
				// this boundary method we need the reference in order to generate the client form!
				final BoundaryMethod uploadMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
				uploadMethod.setComment("Upload binary data that should be stored in attribute \"" + attr.getName() + "\"");
				uploadMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
				uploadMethod.setMethodType(BoundaryMethodTypeEnumeration.UPLOAD);
				uploadMethod.setReturnType(project.getJavaTypeByName(JavaType.VOID));
				uploadMethod.setDomainAttribute(attr);
				uploadMethod.setName(methodName);

				final RepositoryMethod repositoryMethod = boundary.getRepository()
						.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

				uploadMethod.setServiceMethod(repositoryMethod);

				// Add a parameter for the ID
				repositoryMethod.getMethodParameters().forEach(param -> {
					final MethodParameter idParam = JavaFactory.eINSTANCE.createMethodParameter();
					idParam.setName(param.getName());
					idParam.setMethod(uploadMethod);
					idParam.setType(param.getType());
					idParam.setModifier(param.getModifier());

					uploadMethod.getMethodParameters().add(idParam);
				});

				// Add a parameter for the path
				final MethodParameter dataParam = JavaFactory.eINSTANCE.createMethodParameter();
				dataParam.setName("path");
				dataParam.setMethod(uploadMethod);
				dataParam.setType(project.getJavaTypeByName(JavaType.STRING));
				dataParam.setModifier(JavaTypeModifierEnumeration.NONE);

				uploadMethod.getMethodParameters().add(dataParam);

				final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(uploadMethod, boundary);

				if (existingMethod == null) {
					uploadMethod.setBoundaryBean(boundary);
					boundary.getBoundaryMethods().add(uploadMethod);
					action.setBoundaryMethod(uploadMethod);
				}
				else
					action.setBoundaryMethod(existingMethod);
			}

			if (form.getFormType() == FormTypeEnumeration.UPDATE || form.getFormType() == FormTypeEnumeration.READONLY) {
				final String methodName;

				if (attr.getDomainObject().equals(form.getDomainObject()))
					methodName = "download" + attr.getUpperCaseName();
				else
					methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

				final String actionName = methodName + "Action";

				// Add an action to download the data
				final FormAction downloadAction = ClientFactory.eINSTANCE.createFormAction();

				// Add the default roles to the action!
				downloadAction.getRoles().addAll(form.getRoles());

				downloadAction.setDescription("Download " + attr.getLabel());
				downloadAction.setName(actionName);
				downloadAction.setType(ActionType.DOWNLOAD);
				downloadAction.setForm(form);

				form.getActions().add(downloadAction);

				final BoundaryMethod downloadMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
				downloadMethod.setComment("Download binary data that is stored in attribute \"" + attr.getName() + "\"");
				downloadMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
				downloadMethod.setMethodType(BoundaryMethodTypeEnumeration.DOWNLOAD);
				downloadMethod.setReturnType(project.getJavaTypeByName(JavaType.STRING));
				downloadMethod.setDomainAttribute(attr);
				downloadMethod.setName(methodName);

				final RepositoryMethod repositoryMethod = boundary.getRepository()
						.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

				downloadMethod.setServiceMethod(repositoryMethod);

				repositoryMethod.getMethodParameters().forEach(param -> {
					final MethodParameter p = JavaFactory.eINSTANCE.createMethodParameter();
					p.setName(param.getName());
					p.setMethod(downloadMethod);
					p.setType(param.getType());
					p.setModifier(param.getModifier());

					downloadMethod.getMethodParameters().add(p);
				});

				final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(downloadMethod, boundary);

				if (existingMethod == null) {
					downloadMethod.setBoundaryBean(boundary);
					boundary.getBoundaryMethods().add(downloadMethod);
					downloadAction.setBoundaryMethod(downloadMethod);
				}
				else
					downloadAction.setBoundaryMethod(existingMethod);
			}
		});
	}

}
