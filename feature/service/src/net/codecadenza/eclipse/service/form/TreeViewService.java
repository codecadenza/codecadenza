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

import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.FormGeneratorFactory;
import net.codecadenza.eclipse.generator.client.IFormGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.service.mapping.MappingObjectService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.ecore.resource.Resource;

/**
 * <p>
 * Service for tree views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeViewService {
	private TreeView treeView;
	private final Project project;
	private final BoundaryService boundaryService;
	private final RepositoryService repositoryService;
	private final DTOBeanService dtoService;
	private final MappingObjectService mappingObjectService;
	private final HashMap<DomainObject, BoundaryBean> boundaryMap = new HashMap<>();
	private final HashMap<BoundaryMethod, DomainObject> methodMap = new HashMap<>();
	private final HashMap<String, DTOBean> dtoMap = new HashMap<>();
	private AssociationHelper rootAssociationHelper;

	/**
	 * Constructor
	 * @param treeView
	 * @param project
	 */
	public TreeViewService(TreeView treeView, Project project) {
		this(project);

		this.treeView = treeView;

		if (treeView.getRootTreeItem() != null)
			initService(treeView.getRootTreeItem());
	}

	/**
	 * Constructor
	 * @param project
	 */
	public TreeViewService(Project project) {
		this.project = project;
		this.boundaryService = new BoundaryService(project);
		this.repositoryService = new RepositoryService(project);
		this.dtoService = new DTOBeanService(project);
		this.mappingObjectService = new MappingObjectService(project);
	}

	/**
	 * @param rootAssociationHelper
	 */
	public void setRootAssociationHelper(AssociationHelper rootAssociationHelper) {
		this.rootAssociationHelper = rootAssociationHelper;
	}

	/**
	 * Initialize a new tree view object based on the given domain object
	 * @param domainObject
	 * @return the initialized tree view object
	 * @throws Exception if an internal error has occurred
	 */
	public TreeView initTreeView(DomainObject domainObject) throws Exception {
		treeView = ClientFactory.eINSTANCE.createTreeView();
		treeView.setDomainObject(domainObject);

		final DTOBean treeDTO = initRootDTO(domainObject);

		treeView.setDTO(treeDTO);

		final TreeViewItem rootTreeItem = ClientFactory.eINSTANCE.createTreeViewItem();
		rootTreeItem.setItemDTO(treeDTO);
		rootTreeItem.setDropMethod(createChangeParentMethod(domainObject));

		treeView.setRootTreeItem(rootTreeItem);

		// Initialize the search method
		addTreeSearchMethod(domainObject);

		boundaryMap.put(treeView.getDomainObject(), getBoundaryOfDomainObject(domainObject));

		// Add methods to download LOB attributes
		addItemDownloadMethods(domainObject);

		final OneToManyAssociation recursionAssoc = getRecursiveAssoc(domainObject);

		if (recursionAssoc != null)
			treeView.setRecursiveMethod(addRecursiveBoundaryMethod(domainObject, recursionAssoc));

		return treeView;
	}

	/**
	 * Save the tree view
	 * @param edit
	 * @throws IllegalStateException if the tree view could't be saved due to a validation error
	 * @throws Exception if an internal error has occurred
	 */
	public void saveTreeView(boolean edit) throws Exception {
		validateTreeView(edit);

		methodMap.keySet().forEach(method -> {
			final BoundaryBean boundary = boundaryMap.get(methodMap.get(method));

			method.setBoundaryBean(boundary);
			boundary.getBoundaryMethods().add(method);
		});

		// Validate all data transfer objects
		for (final DTOBean dtoBean : dtoMap.values()) {
			final IStatus status = mappingObjectService.validateMappingObject(dtoBean);

			if (!status.isOK())
				throw new IllegalStateException(status.getMessage());
		}

		// Set namespaces for all data transfer objects
		for (final DTOBean dtoBean : dtoMap.values()) {
			for (final Namespace n : project.getDTONamespace().getChildNamespaces())
				if (n.getName().equals(dtoBean.getDomainObject().getNamespace().getName())) {
					dtoBean.setNamespace(n);
					break;
				}
		}

		// Add the data transfer objects to the meta-model
		for (final DTOBean dtoBean : dtoMap.values())
			if (!project.eResource().getContents().contains(dtoBean))
				project.eResource().getContents().add(dtoBean);

		// Add the boundaries to the meta-model
		boundaryMap.values().forEach(b -> {
			final Resource eResource = project.eResource();

			if (!eResource.getContents().contains(b))
				eResource.getContents().add(b);

			if (!eResource.getContents().contains(b.getRepository()))
				eResource.getContents().add(b.getRepository());
		});

		project.eResource().getContents().add(treeView);

		// Synchronize integration beans
		new IntegrationBeanSyncService(project).sync();

		// Save the meta-model
		EclipseIDEService.saveProjectMetaData(project);

		// Build the data transfer objects
		for (final DTOBean dto : dtoMap.values())
			dtoService.rebuildDTOBeanSourceFiles(dto);

		// Build the repositories
		if (project.isBoundaryMode())
			for (final BoundaryBean boundary : boundaryMap.values())
				repositoryService.rebuildRepositorySourceFiles(boundary.getRepository());

		// Build the boundaries
		for (final BoundaryBean boundary : boundaryMap.values())
			boundaryService.rebuildBoundarySourceFiles(boundary);

		if (!edit && project.hasEclipseClient()) {
			final String projectName = project.getTargetProjectName(BuildArtifactType.GUI);
			final String packageName = project.getClientNamespace().toString() + PACK_CLIENT_TREE;

			// Add the tree view to the application model
			EclipseIDEService.addViewToApplicationModel(projectName, treeView.getTitle(), packageName + "." + treeView.getName());
		}

		// Create the tree view source file
		final IFormGenerator formGenerator = FormGeneratorFactory.getFormGenerator(project);
		formGenerator.createForm(treeView);
	}

	/**
	 * Generate the select statement
	 * @param dto
	 * @param subItem
	 * @param recursiveGetter
	 * @param skipSelectClause
	 * @return the generated string
	 */
	public String generateSelectStatement(DTOBean dto, TreeViewItem subItem, boolean recursiveGetter, boolean skipSelectClause) {
		final var b = new StringBuilder();
		final var aliasMap = new HashMap<String, String>();
		var recursionQueryPart = "";

		// We just return an empty string if no attribute exists yet!
		if (dto.getAttributes().isEmpty())
			return "";

		if (!skipSelectClause) {
			boolean firstAttribute = true;

			b.append("select\n");

			for (final DTOBeanAttribute attr : dto.getAttributes()) {
				if (firstAttribute)
					firstAttribute = false;
				else
					b.append(",\n");

				b.append(attr.getSelectToken());

				final String token = attr.getSelectToken().substring(0, attr.getSelectToken().indexOf('.'));

				// Extract the alias in order to put it into the map
				if (!token.equals(AssociationHelper.INITIAL_ALIAS))
					aliasMap.put(token, "");
			}

			b.append("\n");
		}
		else {
			dto.getAttributes().forEach(attr -> {
				final String token = attr.getSelectToken().substring(0, attr.getSelectToken().indexOf('.'));

				// Extract the alias in order to put it into the map
				if (!token.equals(AssociationHelper.INITIAL_ALIAS))
					aliasMap.put(token, "");
			});
		}

		b.append("from ");

		if (subItem != null) {
			b.append(subItem.getAssociation().getDomainObject().getName() + " x join x.");
			b.append(subItem.getAssociation().getName() + " a");
		}
		else {
			final DomainObject domainObject = treeView.getDTO().getDomainObject();
			final OneToManyAssociation recursionAssoc = getRecursiveAssoc(domainObject);

			b.append(domainObject.getName());

			if (recursionAssoc != null) {
				if (!recursiveGetter) {
					b.append(" a");

					final ManyToOneAssociation manyToOne = recursionAssoc.getReverseAssociation();

					if (manyToOne != null && manyToOne.isOptional())
						recursionQueryPart = " where a." + manyToOne.getName() + " is null";
				}
				else
					b.append(" x join x." + recursionAssoc.getName() + " a");
			}
			else
				b.append(" a");
		}

		// Copy all aliases into a list
		final var aliasCollection = new BasicEList<String>();

		aliasMap.keySet().forEach(aliasCollection::add);

		// Sort the aliases
		ECollections.sort(aliasCollection, String::compareTo);

		final var usedJoins = new HashSet<String>();

		// Add join statements for all associations!
		aliasCollection.forEach(alias -> {
			AssociationHelper rootAssociationOfJoin = rootAssociationHelper;

			if (subItem != null)
				rootAssociationOfJoin = rootAssociationHelper.searchAssociationHelper(subItem.getAssociation());

			b.append(rootAssociationOfJoin.generateJoinStatement(alias, usedJoins, false));
		});

		b.append(recursionQueryPart);

		return b.toString();
	}

	/**
	 * @param parentItem
	 * @param parentDomainObject
	 * @param assoc
	 * @return the new sub-tree item
	 * @throws IllegalStateException if the item must not be added to the selected parent item
	 * @throws Exception if an internal error has occurred
	 */
	public TreeViewItem addSubTreeItem(TreeViewItem parentItem, DomainObject parentDomainObject, AbstractDomainAssociation assoc)
			throws Exception {
		final DomainObject selDomainObject = assoc.getTarget();
		final var dtoName = assoc.getUpperCaseName() + "TreeDTO";
		final var dtoKey = selDomainObject.getNamespace().toString() + "." + dtoName;
		DTOBean itemDTO = null;

		// Check if the selected association belongs to the given domain object
		final boolean isValid = parentDomainObject.getAllAssociations().stream().anyMatch(as -> as.equals(assoc));

		if (!isValid)
			throw new IllegalStateException("The creation of an item is disallowed for this location!");

		String itemLabel = EclipseIDEService.buildDefaultLabel(assoc.getName());
		itemLabel = itemLabel.substring(0, 1).toUpperCase() + itemLabel.substring(1);

		if (!dtoMap.containsKey(dtoKey)) {
			itemDTO = DtoFactory.eINSTANCE.createDTOBean();
			itemDTO.setDomainObject(selDomainObject);
			itemDTO.setComment("DTO for " + selDomainObject.getLabel().toLowerCase() + " objects");
			itemDTO.setName(dtoName);
			itemDTO.setStandardConversion(false);

			dtoMap.put(dtoKey, itemDTO);
		}
		else
			itemDTO = dtoMap.get(dtoKey);

		// Create a sub-item
		final TreeViewItem subTreeItem = ClientFactory.eINSTANCE.createTreeViewItem();
		subTreeItem.setLabel(itemLabel);
		subTreeItem.setItemDTO(itemDTO);
		subTreeItem.setAssociation(assoc);
		subTreeItem.setDropMethod(addDropMethod(assoc));

		final BoundaryMethod m = addSubItemBoundaryFetchMethod(parentDomainObject, itemDTO, assoc);
		subTreeItem.setDataFetchMethod(m);

		// Add further download methods for all LOB attributes
		addItemDownloadMethods(selDomainObject);

		parentItem.getChildren().add(subTreeItem);
		subTreeItem.setParentItem(parentItem);

		return subTreeItem;
	}

	/**
	 * Create a new DTO attribute based on the provided domain attribute. A new attribute won't be created if an attribute with the
	 * same name already exists!
	 * @param dto the data transfer object
	 * @param attribute the attribute to be added
	 * @param assocList the association list
	 * @return either the new or an existing DTO attribute
	 */
	public DTOBeanAttribute addAttributeToDTO(DTOBean dto, DomainAttribute attribute, List<AbstractDomainAssociation> assocList) {
		return dto.addAttribute(attribute, assocList, true);
	}

	/**
	 * Safely remove the given attribute from the corresponding DTO
	 * @param attr
	 * @param treeViewItem
	 */
	public void removeDTOAttribute(DTOBeanAttribute attr, TreeViewItem treeViewItem) {
		final DTOBean dto = attr.getDTOBean();
		int count = 0;

		// Perform some tests in order to remove the attribute only if it isn't used by other tree view objects!
		if (treeViewItem.isRootItem()) {
			for (final TreeSearchItem item : treeView.getQuickSearchItems())
				if (item.getDTOAttribute().equals(attr))
					count++;

			for (final TreeSearchItem item : treeView.getAdvancedSearchItems())
				if (item.getDTOAttribute().equals(attr))
					count++;
		}

		for (final TreeNode node : treeViewItem.getNodes())
			if (node.getDTOAttribute().equals(attr))
				count++;

		for (final DTOBeanAttribute a : treeViewItem.getDisplayAttributes())
			if (a.equals(attr))
				count++;

		for (final DTOBeanAttribute a : treeViewItem.getInvisibleAttributes())
			if (a.equals(attr))
				count++;

		if (count == 1)
			dto.getAttributes().remove(attr);
	}

	/**
	 * Create an initial label for a tree view item based on the provided domain attribute
	 * @param attribute the attribute
	 * @param assocList the association list
	 * @return the created label
	 */
	public String createInitialAttributeLabel(DomainAttribute attribute, List<AbstractDomainAssociation> assocList) {
		if (assocList == null || assocList.isEmpty()) {
			final String attrLabel = attribute.getLabel();

			return attrLabel.substring(0, 1).toUpperCase() + attrLabel.substring(1);
		}

		final AbstractDomainAssociation a = assocList.get(0);
		final String attrLabel = attribute.getLabel();
		final String domainObjectLabel = a.getTarget().getLabel();

		return domainObjectLabel.substring(0, 1).toUpperCase() + domainObjectLabel.substring(1) + " " + attrLabel;
	}

	/**
	 * Initialize an item that is used for either a quick or an advanced search operation
	 * @param dto
	 * @param domainAttribute
	 * @param assocList
	 * @param selectToken
	 * @return the initialized tree search item
	 */
	public TreeSearchItem initTreeSearchItem(DTOBean dto, DomainAttribute domainAttribute,
			List<AbstractDomainAssociation> assocList, String selectToken) {
		final DTOBeanAttribute attr = addAttributeToDTO(dto, domainAttribute, assocList);
		attr.setSelectToken(selectToken);

		final TreeSearchItem item = ClientFactory.eINSTANCE.createTreeSearchItem();
		item.setDTOAttribute(attr);
		item.setLabel(createInitialAttributeLabel(attr.getDomainAttribute(), assocList));

		return item;
	}

	/**
	 * Synchronize the tree view boundary method
	 * @param dataFetchType
	 * @throws Exception if an internal error has occurred
	 */
	public void syncTreeMethod(BoundaryMethodDataFetchType dataFetchType) throws Exception {
		final DomainObject domainObject = treeView.getDTO().getDomainObject();
		final BoundaryMethod treeMethod = treeView.getBoundaryMethod();
		final BoundaryBean treeBoundary = getBoundaryOfDomainObject(domainObject);
		final boolean hasSearchItems = !treeView.getAdvancedSearchItems().isEmpty() || !treeView.getQuickSearchItems().isEmpty();

		if (treeView.getRecursiveMethod() == null && !hasSearchItems && dataFetchType == BoundaryMethodDataFetchType.DEFAULT) {
			treeMethod.setComment("Get all " + domainObject.getLabel() + " objects");
			treeMethod.setMethodType(BoundaryMethodTypeEnumeration.FIND_ALL);
			treeMethod.setServiceMethod(treeBoundary.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.FIND_ALL));
		}
		else {
			treeMethod.setComment("Search for " + domainObject.getLabel() + " objects");
			treeMethod.setMethodType(BoundaryMethodTypeEnumeration.SEARCH);
			treeMethod.setServiceMethod(treeBoundary.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.SEARCH));
		}
	}

	/**
	 * Create a tree view boundary method for performing a count operation
	 * @param dataFetchType
	 * @return the count method or null if this method is not necessary
	 * @throws Exception if an internal error has occurred
	 */
	public BoundaryMethod addTreeCountMethod(BoundaryMethodDataFetchType dataFetchType) throws Exception {
		final DomainObject domainObject = treeView.getDTO().getDomainObject();
		final BoundaryBean treeBoundary = getBoundaryOfDomainObject(domainObject);

		// Don't create a new count method if the tree view already has one!
		if (treeView.getCountMethod() != null)
			return treeView.getCountMethod();

		// A count method is not necessary if no advanced search items exist!
		if (treeView.getAdvancedSearchItems().isEmpty())
			return null;

		final RepositoryMethod repositoryMethod = treeBoundary.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.COUNT);

		final BoundaryMethod countMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		countMethod.setServiceMethod(repositoryMethod);
		countMethod.setComment("Count " + domainObject.getLabel() + " objects");
		countMethod.setMethodType(BoundaryMethodTypeEnumeration.COUNT);
		countMethod.setReturnType(project.getJavaTypeByName(JavaType.LONG));
		countMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		countMethod.setQueryStatement(treeView.getBoundaryMethod().getQueryStatement());
		countMethod.setCustomStatement(treeView.getBoundaryMethod().getCustomStatement());
		countMethod.setDataFetchType(dataFetchType);

		treeView.setCountMethod(countMethod);

		methodMap.put(countMethod, domainObject);

		return countMethod;
	}

	/**
	 * Check if a tree search item may be added
	 * @return true if adding is allowed
	 */
	public boolean canAddSearchItem() {
		// In any case, items can be added as long as the signature of the tree view boundary method won't be changed!
		if (treeView.needsSearchObject())
			return true;

		// Search for a REST service bean that uses the same boundary as the tree view
		final AbstractIntegrationBean restBean = project.searchIntegrationBean(IntegrationTechnology.REST,
				treeView.getDomainObject());

		if (restBean != null) {
			final Stream<AbstractIntegrationMethod> methods = restBean.getMethods().stream();

			// The item must not be added if a corresponding REST method already exists!
			return methods.noneMatch(integrationMethod -> integrationMethod.getBoundaryMethod().equals(treeView.getBoundaryMethod()));
		}

		return true;
	}

	/**
	 * Check if a tree search item may be removed
	 * @return true if removing is allowed
	 */
	public boolean canRemoveSearchItem() {
		// In any case, items can be deleted as long as the signature of the tree view boundary method won't be changed!
		if (treeView.getRecursiveMethod() != null)
			return true;

		if (treeView.getBoundaryMethod().getDataFetchType() != BoundaryMethodDataFetchType.DEFAULT)
			return true;

		if (treeView.getAdvancedSearchItems().size() + treeView.getQuickSearchItems().size() > 1)
			return true;

		// Search for a REST service bean that uses the same boundary as the tree view
		final AbstractIntegrationBean restBean = project.searchIntegrationBean(IntegrationTechnology.REST,
				treeView.getDomainObject());

		if (restBean == null)
			return true;

		final Stream<AbstractIntegrationMethod> methods = restBean.getMethods().stream();

		// The item must not be removed if a corresponding REST method already exists!
		return methods.noneMatch(integrationMethod -> integrationMethod.getBoundaryMethod().equals(treeView.getBoundaryMethod()));
	}

	/**
	 * Check if the tree view meta-data is valid
	 * @param edit
	 * @throws IllegalStateException if the tree view meta-data is invalid
	 */
	private void validateTreeView(boolean edit) {
		// Verify that every DTO has a primary key attribute
		for (final DTOBean b : dtoMap.values())
			if (b.getPKAttribute() == null)
				throw new IllegalStateException("The primary key attribute of the DTO '" + b.getName() + "' could not be found!");

		// Check if every tree item has one display attribute at least!
		if (treeView.getRootTreeItem().getDisplayAttributes().isEmpty())
			throw new IllegalStateException("There must be one display attribute for the root tree item at least!");

		final var assocNames = treeView.getAllSubTreeItems().stream().map(TreeViewItem::getAssociation)
				.map(AbstractDomainAssociation::getName).toList();

		for (final String assocName : assocNames) {
			final long counter = treeView.getAllSubTreeItems().stream().map(TreeViewItem::getAssociation)
					.map(AbstractDomainAssociation::getName).filter(a -> a.equals(assocName)).count();

			if (counter > 1) {
				final var message = new StringBuilder("The names of the associations added to the tree must be unique! ");
				message.append("But there are associations with the same name '");
				message.append(assocName);
				message.append("'!");

				throw new IllegalStateException(message.toString());
			}
		}

		final boolean displayCheck = checkDisplayAttributes();

		if (!displayCheck)
			throw new IllegalStateException("There must be one display attribute for every tree item at least!");

		// Check all data transfer objects and change their names if necessary
		dtoMap.values().forEach(this::adjustDTOBeanName);

		// Check if a view form with the same name already exists
		if (!edit)
			for (final Form f : project.getAllFormsOfProject()) {
				final FormTypeEnumeration t = f.getFormType();

				if (t != FormTypeEnumeration.SEARCHABLE_VIEW && t != FormTypeEnumeration.SIMPLE_VIEW
						&& t != FormTypeEnumeration.TREE_VIEW)
					continue;

				if (treeView.equals(f))
					continue;

				if (treeView.getName().equals(f.getName()))
					throw new IllegalStateException("A view with this name already exists!");
			}

		// Some specific tree view methods must not to be reused!
		for (final Map.Entry<BoundaryMethod, DomainObject> entry : methodMap.entrySet()) {
			final BoundaryBean boundary = boundaryMap.get(entry.getValue());
			final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(entry.getKey(), boundary);

			if (existingMethod != null)
				throw new IllegalStateException("A boundary method with the name '" + entry.getKey().getName() + "' already exists!");
		}
	}

	/**
	 * Initialize the data transfer object for root tree items
	 * @param domainObject
	 * @return the new data transfer object
	 */
	private DTOBean initRootDTO(DomainObject domainObject) {
		final var dtoName = domainObject.getUpperCaseName() + "TreeDTO";

		final DTOBean treeDTO = DtoFactory.eINSTANCE.createDTOBean();
		treeDTO.setDomainObject(domainObject);
		treeDTO.setComment("DTO for " + domainObject.getLabel().toLowerCase() + " objects");
		treeDTO.setName(dtoName);
		treeDTO.setStandardConversion(false);

		final var dtoKey = domainObject.getNamespace().toString() + "." + dtoName;
		dtoMap.put(dtoKey, treeDTO);

		return treeDTO;
	}

	/**
	 * Recursive method for initializing internal data structures
	 * @param treeViewItem
	 */
	private void initService(TreeViewItem treeViewItem) {
		if (treeViewItem.isRootItem())
			boundaryMap.put(treeView.getDomainObject(), treeView.getBoundaryMethod().getBoundaryBean());

		final var dtoKey = treeViewItem.getItemDTO().getNamespace().toString() + "." + treeViewItem.getItemDTO().getName();
		dtoMap.put(dtoKey, treeViewItem.getItemDTO());

		treeViewItem.getChildren().forEach(subItem -> {
			final DomainObject parentDomainObject = subItem.getDataFetchMethod().getBoundaryBean().getDomainObject();
			boundaryMap.put(parentDomainObject, subItem.getDataFetchMethod().getBoundaryBean());

			initService(subItem);
		});
	}

	/**
	 * Check if every tree item has one display attribute at least!
	 * @return true if all tree items have a display attribute
	 */
	private boolean checkDisplayAttributes() {
		return checkDisplayAttributes(treeView.getRootTreeItem().getChildren());
	}

	/**
	 * Recursive method to check if every tree item has one display attribute at least!
	 * @param items
	 * @return true if all tree items have a display attribute
	 */
	private boolean checkDisplayAttributes(Collection<TreeViewItem> items) {
		for (final TreeViewItem treeItem : items) {
			if (treeItem.getDisplayAttributes().isEmpty())
				return false;

			if (!checkDisplayAttributes(treeItem.getChildren()))
				return false;
		}

		return true;
	}

	/**
	 * Check if a bidirectional one-to-many association exists that can be used for recursion. Note that only one association can be
	 * detected and used for the recursion! Furthermore it is important that the corresponding many-to-one association must be
	 * optional! If more than one association exists the first association within the list will be returned randomly!
	 * @param domainObject
	 * @return the association that can be used for the recursion
	 */
	private OneToManyAssociation getRecursiveAssoc(DomainObject domainObject) {
		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
			if (assoc instanceof final OneToManyAssociation oneToMany && oneToMany.getTarget().equals(domainObject)) {
				final ManyToOneAssociation manyToOne = oneToMany.getReverseAssociation();

				if (manyToOne != null && manyToOne.isOptional())
					return oneToMany;
			}

		return null;

	}

	/**
	 * Adjust the name of a given DTO. The method will invoke itself as long as no duplicate name has been found!
	 * @param dto
	 */
	private void adjustDTOBeanName(DTOBean dto) {
		for (final DTOBean p : project.getAllDTOsOfProject()) {
			if (dto.equals(p))
				continue;

			if (dto.getName().equals(p.getName())
					&& dto.getDomainObject().getNamespace().toString().equals(p.getDomainObject().getNamespace().toString())) {
				final DomainObject domainObj = dto.getDomainObject();
				final String oldName = dto.getName();
				final String counter = oldName.substring(oldName.length() - 1);
				int treeCounter = 1;

				try {
					treeCounter = Integer.parseInt(counter) + 1;
				}
				catch (final NumberFormatException e) {
					// This exception will be ignored!
				}

				final var newName = domainObj.getUpperCaseName() + "TreeDTO" + treeCounter;
				dto.setName(newName);
				adjustDTOBeanName(dto);
				break;
			}
		}
	}

	/**
	 * Search a boundary bean for the given domain object and create it if it doesn't exist yet!
	 * @param domainObject
	 * @return either a new or an existing boundary bean
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryBean getBoundaryOfDomainObject(DomainObject domainObject) throws Exception {
		BoundaryBean boundary = null;

		if (!boundaryMap.containsKey(domainObject)) {
			boundary = boundaryService.getOrCreateBoundaryOfDomainObject(domainObject);

			boundaryMap.put(domainObject, boundary);
		}
		else
			boundary = boundaryMap.get(domainObject);

		return boundary;
	}

	/**
	 * Add download methods based on the given domain object
	 * @param domainObject
	 * @throws Exception if an internal error has occurred
	 */
	private void addItemDownloadMethods(DomainObject domainObject) throws Exception {
		final BoundaryBean boundary = getBoundaryOfDomainObject(domainObject);
		final RepositoryMethod repositoryMethod = boundary.getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

		domainObject.getAllLobAttributes().forEach(attr -> {
			final String methodName;

			if (attr.getDomainObject().equals(domainObject))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final BoundaryMethod downloadMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
			downloadMethod.setComment("Download binary data that is stored in attribute \"" + attr.getName() + "\"");
			downloadMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			downloadMethod.setMethodType(BoundaryMethodTypeEnumeration.DOWNLOAD);
			downloadMethod.setReturnType(project.getJavaTypeByName(JavaType.STRING));
			downloadMethod.setDomainAttribute(attr);
			downloadMethod.setName(methodName);
			downloadMethod.setServiceMethod(repositoryMethod);

			repositoryMethod.getMethodParameters().forEach(param -> {
				final MethodParameter p = JavaFactory.eINSTANCE.createMethodParameter();
				p.setName(param.getName());
				p.setMethod(downloadMethod);
				p.setType(param.getType());
				p.setModifier(param.getModifier());

				downloadMethod.getMethodParameters().add(p);
			});

			addBoundaryMethod(boundary, downloadMethod);
		});
	}

	/**
	 * Add a boundary method for fetching the sub-items
	 * @param domainObject
	 * @param dto
	 * @param assoc
	 * @return the new boundary method
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod addSubItemBoundaryFetchMethod(DomainObject domainObject, DTOBean dto, AbstractDomainAssociation assoc)
			throws Exception {
		final BoundaryBean subItemBoundary = getBoundaryOfDomainObject(domainObject);
		final var methodName = "get" + assoc.getUpperCaseName() + "Of" + domainObject.getUpperCaseName() + "ForTree";
		final RepositoryMethod repositoryMethod = subItemBoundary.getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.SEARCH);
		final DomainAttribute pkAttribute = domainObject.getPKAttribute();

		final BoundaryMethod getterMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		getterMethod.setName(methodName);
		getterMethod.setComment("Get all " + assoc.getTarget().getLabelPlural() + " of the given " + domainObject.getLabel());
		getterMethod.setReturnType(dto);
		getterMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		getterMethod.setMethodType(BoundaryMethodTypeEnumeration.FIND_BY_PARENT);
		getterMethod.setCustomStatement("");
		getterMethod.setQueryStatement("from " + domainObject.getName() + " x join x." + assoc.getName() + " a");
		getterMethod.setServiceMethod(repositoryMethod);

		final MethodParameter paramGetter = JavaFactory.eINSTANCE.createMethodParameter();
		paramGetter.setName(pkAttribute.getName());
		paramGetter.setType(pkAttribute.getJavaType());
		paramGetter.setMethod(getterMethod);
		paramGetter.setModifier(JavaTypeModifierEnumeration.NONE);

		getterMethod.getMethodParameters().add(paramGetter);

		methodMap.put(getterMethod, domainObject);

		return getterMethod;
	}

	/**
	 * Create a boundary method for either adding an object to a many-to-many association or for changing the parent of an object
	 * that belongs to a one-to-many association when performing a drag and drop operation of the respective tree item
	 * @param assoc
	 * @return either a new or an existing boundary method. It will return null if no respective method either has been found or
	 *         created!
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod addDropMethod(AbstractDomainAssociation assoc) throws Exception {
		if (assoc instanceof final OneToManyAssociation oneToMany && oneToMany.isBidirectional())
			return createChangeParentMethod(oneToMany);
		else if (assoc instanceof final OneToManyAssociation oneToMany && !oneToMany.isBidirectional()) {
			createRemoveFromAssociationMethod(assoc);

			return createChangeAssociationMethod(oneToMany);
		}
		else if (assoc instanceof final ManyToManyAssociation mtm && mtm.isOwner()) {
			createRemoveFromAssociationMethod(assoc);

			return createAddToAssociationMethod(mtm);
		}

		return null;
	}

	/**
	 * Add a boundary method for removing references from either a many-to-many or a unidirectional one-to-many association
	 * @param assoc
	 * @throws Exception if an internal error has occurred
	 */
	private void createRemoveFromAssociationMethod(AbstractDomainAssociation assoc) throws Exception {
		final BoundaryBean boundary = getBoundaryOfDomainObject(assoc.getDomainObject());
		final var methodName = "remove" + assoc.getTarget().getUpperCaseName() + "From" + assoc.getUpperCaseName();
		final DomainAttribute pkSource = assoc.getDomainObject().getPKAttribute();
		final DomainAttribute pkTarget = assoc.getTarget().getPKAttribute();
		final RepositoryMethod repositoryMethod = boundary.getRepository()
				.getMethodByTypeAndAssociation(RepositoryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION, assoc);
		var comment = "Remove the " + assoc.getTarget().getLabel() + " from the association list of the given ";
		comment += assoc.getDomainObject().getLabel();

		if (repositoryMethod == null)
			return;

		final BoundaryMethod methodRemoveFromAssoc = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		methodRemoveFromAssoc.setComment(comment);
		methodRemoveFromAssoc.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodRemoveFromAssoc.setMethodType(BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION);
		methodRemoveFromAssoc.setReturnType(project.getJavaTypeByName(JavaType.VOID));
		methodRemoveFromAssoc.setAssociation(assoc);
		methodRemoveFromAssoc.setName(methodName);
		methodRemoveFromAssoc.setServiceMethod(repositoryMethod);

		final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
		paramId.setName(pkSource.getName());
		paramId.setMethod(methodRemoveFromAssoc);
		paramId.setType(pkSource.getJavaType());
		paramId.setModifier(JavaTypeModifierEnumeration.NONE);

		final MethodParameter paramRef = JavaFactory.eINSTANCE.createMethodParameter();
		paramRef.setName(pkTarget.getName());
		paramRef.setMethod(methodRemoveFromAssoc);
		paramRef.setType(pkTarget.getJavaType());
		paramRef.setModifier(JavaTypeModifierEnumeration.NONE);

		if (paramRef.getName().equals(paramId.getName()))
			paramRef.setName(assoc.getTarget().getLowerCaseName());

		methodRemoveFromAssoc.getMethodParameters().add(paramId);
		methodRemoveFromAssoc.getMethodParameters().add(paramRef);

		addBoundaryMethod(boundary, methodRemoveFromAssoc);
	}

	/**
	 * Add a boundary method for adding a reference to a many-to-many association
	 * @param assoc
	 * @return a boundary method for adding a reference to a many-to-many association. It will return null if no respective method
	 *         either has been found or created!
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod createAddToAssociationMethod(AbstractDomainAssociation assoc) throws Exception {
		final BoundaryBean boundary = getBoundaryOfDomainObject(assoc.getDomainObject());
		final var methodName = "add" + assoc.getTarget().getUpperCaseName() + "To" + assoc.getUpperCaseName();
		final DomainAttribute pkSource = assoc.getDomainObject().getPKAttribute();
		final DomainAttribute pkTarget = assoc.getTarget().getPKAttribute();
		final RepositoryMethod repositoryMethod = boundary.getRepository()
				.getMethodByTypeAndAssociation(RepositoryMethodTypeEnumeration.ADD_TO_ASSOCIATION, assoc);
		final var comment = "Add the " + assoc.getTarget().getLabel() + " to the association list of the given "
				+ assoc.getDomainObject().getLabel();

		if (repositoryMethod == null)
			return null;

		final BoundaryMethod methodAddToAssoc = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		methodAddToAssoc.setComment(comment);
		methodAddToAssoc.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodAddToAssoc.setMethodType(BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION);
		methodAddToAssoc.setReturnType(project.getJavaTypeByName(JavaType.VOID));
		methodAddToAssoc.setAssociation(assoc);
		methodAddToAssoc.setName(methodName);
		methodAddToAssoc.setServiceMethod(repositoryMethod);

		final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
		paramId.setName(pkSource.getName());
		paramId.setMethod(methodAddToAssoc);
		paramId.setType(pkSource.getJavaType());
		paramId.setModifier(JavaTypeModifierEnumeration.NONE);

		final MethodParameter paramRef = JavaFactory.eINSTANCE.createMethodParameter();
		paramRef.setName(pkTarget.getName());
		paramRef.setMethod(methodAddToAssoc);
		paramRef.setType(pkTarget.getJavaType());
		paramRef.setModifier(JavaTypeModifierEnumeration.NONE);

		if (paramRef.getName().equals(paramId.getName()))
			paramRef.setName(assoc.getTarget().getLowerCaseName());

		methodAddToAssoc.getMethodParameters().add(paramId);
		methodAddToAssoc.getMethodParameters().add(paramRef);

		return addBoundaryMethod(boundary, methodAddToAssoc);
	}

	/**
	 * Create a boundary method for changing the parent of an object that belongs to a one-to-many association when performing a
	 * drag and drop operation of the respective tree item
	 * @param otm
	 * @return either a new or an existing boundary method. It will return null if no respective method either has been found or
	 *         created!
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod createChangeParentMethod(OneToManyAssociation otm) throws Exception {
		final ManyToOneAssociation mtmToChange = otm.getReverseAssociation();
		final DomainObject domainObject = otm.getDomainObject();
		final DomainObject reverseDomainObj = otm.getTarget();
		final BoundaryBean boundary = getBoundaryOfDomainObject(reverseDomainObj);
		final RepositoryMethod repositoryMethod = boundary.getRepository()
				.getMethodByTypeAndAssociation(RepositoryMethodTypeEnumeration.CHANGE_PARENT, mtmToChange);

		if (mtmToChange == null || !mtmToChange.isUpdatable() || repositoryMethod == null)
			return null;

		final String domainName = reverseDomainObj.getUpperCaseName();
		final var methodName = "change" + mtmToChange.getUpperCaseName() + "Of" + domainName;

		final BoundaryMethod changeMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		changeMethod.setName(methodName);
		changeMethod.setComment("Change the " + domainObject.getLabel() + " of the given " + reverseDomainObj.getLabel());
		changeMethod.setReturnType(project.getJavaTypeByName(JavaType.VOID));
		changeMethod.setMethodType(BoundaryMethodTypeEnumeration.CHANGE_PARENT);
		changeMethod.setAssociation(mtmToChange);
		changeMethod.setServiceMethod(repositoryMethod);

		final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
		paramId.setName(reverseDomainObj.getPKAttribute().getName());
		paramId.setType(reverseDomainObj.getPKAttribute().getJavaType());
		paramId.setMethod(changeMethod);
		paramId.setModifier(JavaTypeModifierEnumeration.NONE);

		final MethodParameter paramAssocId = JavaFactory.eINSTANCE.createMethodParameter();
		paramAssocId.setName(domainObject.getPKAttribute().getName());
		paramAssocId.setType(domainObject.getPKAttribute().getJavaType());
		paramAssocId.setMethod(changeMethod);
		paramAssocId.setModifier(JavaTypeModifierEnumeration.NONE);

		if (paramId.getName().equals(paramAssocId.getName()))
			paramAssocId.setName(domainObject.getLowerCaseName());

		changeMethod.getMethodParameters().add(paramId);
		changeMethod.getMethodParameters().add(paramAssocId);

		return addBoundaryMethod(boundary, changeMethod);
	}

	/**
	 * Create a boundary method for moving a referenced target object of a unidirectional one-to-many association
	 * @param otm
	 * @return either a new or an existing boundary method. It will return null if no respective method either has been found or
	 *         created!
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod createChangeAssociationMethod(OneToManyAssociation otm) throws Exception {
		final DomainObject sourceDomainObject = otm.getDomainObject();
		final DomainObject targetDomainObject = otm.getTarget();
		final BoundaryBean boundary = getBoundaryOfDomainObject(sourceDomainObject);
		final var methodName = "move" + targetDomainObject.getUpperCaseName() + "To" + sourceDomainObject.getUpperCaseName();
		final RepositoryMethod repositoryMethod = boundary.getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

		if (repositoryMethod == null)
			return null;

		final BoundaryMethod changeMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		changeMethod.setName(methodName);
		changeMethod.setComment("Add the " + targetDomainObject.getLabel() + " to the given " + sourceDomainObject.getLabel());
		changeMethod.setReturnType(project.getJavaTypeByName(JavaType.VOID));
		changeMethod.setMethodType(BoundaryMethodTypeEnumeration.CHANGE_ASSOCIATION);
		changeMethod.setAssociation(otm);
		changeMethod.setServiceMethod(repositoryMethod);

		final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
		paramId.setName(targetDomainObject.getPKAttribute().getName());
		paramId.setType(targetDomainObject.getPKAttribute().getJavaType());
		paramId.setMethod(changeMethod);
		paramId.setModifier(JavaTypeModifierEnumeration.NONE);

		final MethodParameter paramAssocId = JavaFactory.eINSTANCE.createMethodParameter();
		paramAssocId.setName(sourceDomainObject.getPKAttribute().getName());
		paramAssocId.setType(sourceDomainObject.getPKAttribute().getJavaType());
		paramAssocId.setMethod(changeMethod);
		paramAssocId.setModifier(JavaTypeModifierEnumeration.NONE);

		if (paramId.getName().equals(paramAssocId.getName()))
			paramAssocId.setName(sourceDomainObject.getLowerCaseName());

		changeMethod.getMethodParameters().add(paramId);
		changeMethod.getMethodParameters().add(paramAssocId);

		// Make sure that the method name is unique
		for (final BoundaryMethod existingMethod : boundary.getBoundaryMethods())
			if (existingMethod.getName().equals(changeMethod.getName()) && !otm.equals(existingMethod.getAssociation()))
				changeMethod.setName("update" + otm.getUpperCaseName());

		return addBoundaryMethod(boundary, changeMethod);
	}

	/**
	 * Create a boundary method for changing the parent of an object that belongs to a one-to-many association when performing a
	 * drag and drop operation of the respective root tree item
	 * @param domainObject
	 * @return either a new or an existing boundary method. It will return null if no respective method either has been found or
	 *         created!
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod createChangeParentMethod(DomainObject domainObject) throws Exception {
		final OneToManyAssociation recursiveOneToMany = getRecursiveAssoc(domainObject);

		if (recursiveOneToMany == null)
			return null;

		return createChangeParentMethod(recursiveOneToMany);
	}

	/**
	 * Create a new boundary method that is used for fetching items of recursive tree view structures
	 * @param domainObject
	 * @param recursionAssoc
	 * @return the new boundary method
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod addRecursiveBoundaryMethod(DomainObject domainObject, OneToManyAssociation recursionAssoc)
			throws Exception {
		final BoundaryBean recursiveBoundary = getBoundaryOfDomainObject(domainObject);
		final var methodName = "find" + recursionAssoc.getUpperCaseName() + "Of" + domainObject.getUpperCaseName() + "ForTree";

		final BoundaryMethod recursiveGetter = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		recursiveGetter.setName(methodName);
		recursiveGetter.setComment("Get " + domainObject.getLabel() + " objects");
		recursiveGetter.setReturnType(treeView.getDTO());
		recursiveGetter.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		recursiveGetter.setMethodType(BoundaryMethodTypeEnumeration.FIND_BY_PARENT);
		recursiveGetter.setCustomStatement("");

		final RepositoryMethod repositoryMethod = recursiveBoundary.getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.SEARCH);
		recursiveGetter.setServiceMethod(repositoryMethod);

		final DomainAttribute pkAttribute = domainObject.getPKAttribute();

		// Add the method parameter for the primary key attribute
		final MethodParameter paramGetter = JavaFactory.eINSTANCE.createMethodParameter();
		paramGetter.setName(pkAttribute.getName());
		paramGetter.setType(pkAttribute.getJavaType());
		paramGetter.setMethod(recursiveGetter);
		paramGetter.setModifier(JavaTypeModifierEnumeration.NONE);

		recursiveGetter.getMethodParameters().add(paramGetter);

		methodMap.put(recursiveGetter, domainObject);

		return recursiveGetter;
	}

	/**
	 * Create a boundary method for searching root tree items
	 * @param domainObject
	 * @return the new boundary method
	 * @throws Exception if an internal error has occurred
	 */
	private BoundaryMethod addTreeSearchMethod(DomainObject domainObject) throws Exception {
		final BoundaryBean boundary = getBoundaryOfDomainObject(domainObject);
		final var methodName = "search" + domainObject.getNamePlural() + "ForTree";

		final BoundaryMethod searchMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		searchMethod.setName(methodName);
		searchMethod.setComment("Search for " + domainObject.getLabel() + " objects");
		searchMethod.setReturnType(treeView.getDTO());
		searchMethod.setMethodType(BoundaryMethodTypeEnumeration.SEARCH);
		searchMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		searchMethod.setQueryStatement("");
		searchMethod.setCustomStatement("");

		final RepositoryMethod repositoryMethod = boundary.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.SEARCH);
		searchMethod.setServiceMethod(repositoryMethod);

		treeView.setBoundaryMethod(searchMethod);

		methodMap.put(searchMethod, domainObject);

		return searchMethod;
	}

	/**
	 * Add the given boundary method to the boundary bean if it doesn't already exist
	 * @param boundaryBean
	 * @param boundaryMethod
	 * @return the given boundary method or the existing method
	 */
	private BoundaryMethod addBoundaryMethod(BoundaryBean boundaryBean, BoundaryMethod boundaryMethod) {
		final BoundaryMethod existingMethod = boundaryService.checkBoundaryMethod(boundaryMethod, boundaryBean);

		if (existingMethod == null) {
			boundaryMethod.setBoundaryBean(boundaryBean);
			boundaryBean.getBoundaryMethods().add(boundaryMethod);

			return boundaryMethod;
		}

		return existingMethod;
	}

}
