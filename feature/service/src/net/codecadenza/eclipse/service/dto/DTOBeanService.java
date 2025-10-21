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
package net.codecadenza.eclipse.service.dto;

import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_DTO_NAME;
import static net.codecadenza.eclipse.shared.Constants.LIST_DTO_SUFFIX;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.dto.DTOBeanGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service for data transfer objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DTOBeanService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public DTOBeanService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild the DTO source file
	 * @param dtoBean
	 * @throws Exception if the DTO source file could not be rebuilt
	 */
	public void rebuildDTOBeanSourceFiles(DTOBean dtoBean) throws Exception {
		if (dtoBean.isVirtual())
			return;

		new DTOBeanGenerator(dtoBean).createSourceFile();
	}

	/**
	 * Create the log-on DTO
	 * @param baseObject
	 * @param roleAssoc
	 * @param roleDTO
	 * @return the created DTO
	 */
	public DTOBean createLogOnDTO(DomainObject baseObject, AbstractDomainAssociation roleAssoc, DTOBean roleDTO) {
		final DTOBean dtoBean = DtoFactory.eINSTANCE.createDTOBean();
		dtoBean.setName(APP_LOGON_DTO_NAME);
		dtoBean.setDomainObject(baseObject);
		dtoBean.setMappable(false);
		dtoBean.setPrimitive(false);
		dtoBean.setStandardConversion(true);
		dtoBean.setComment("DTO bean for transferring log-on data");

		// Set the proper namespace!
		for (final Namespace n : project.getDTONamespace().getChildNamespaces()) {
			if (n.getName().equals(baseObject.getNamespace().getName())) {
				dtoBean.setNamespace(n);
				n.getJavaTypes().add(dtoBean);
				break;
			}
		}

		dtoBean.addAttribute(roleDTO, roleAssoc, false);

		boolean pkAttrExists = false;

		// Add the attributes
		for (final DomainAttribute attr : baseObject.getAttributes()) {
			if (attr.getTag() == AttributeTagEnumeration.NONE)
				continue;

			dtoBean.addAttribute(attr, false);

			if (!pkAttrExists)
				pkAttrExists = attr.isPk();
		}

		if (!pkAttrExists) {
			final DomainAttribute pkAttr = baseObject.getPKAttribute();

			dtoBean.addAttribute(pkAttr, false);
		}

		// We must add an attribute to save the ID of the client if the project supports mandating!
		if (project.getDomainObjectByTag(DomainTagEnumeration.CLIENT) != null)
			for (final AbstractDomainAssociation assoc : baseObject.getAssociations())
				if (assoc.getTag() == AssociationTagEnumeration.CLIENT_REFERENCE
						&& assoc.getTarget().equals(project.getDomainObjectByTag(DomainTagEnumeration.CLIENT))) {
					dtoBean.addAttribute(assoc.getTarget().getPKAttribute(), assoc, false);
					break;
				}

		dtoBean.getNamespace().getJavaTypes().add(dtoBean);
		project.eResource().getContents().add(dtoBean);

		return dtoBean;
	}

	/**
	 * Remove the given DTO
	 * @param dtoBean
	 * @throws IllegalStateException if the DTO must not be removed as it is referenced by other objects of the internal meta-model
	 * @throws Exception if the DTO source file could not be deleted
	 */
	public void removeDTOBean(DTOBean dtoBean) throws Exception {
		final var forms = new ArrayList<String>();
		final var beans = new ArrayList<String>();
		final var dtos = new ArrayList<String>();

		for (final Form f : project.getAllFormsOfProject())
			if (f.getDTO() != null && f.getDTO().equals(dtoBean))
				forms.add(f.getName());

		for (final BoundaryBean b : project.getAllBoundariesOfProject())
			for (final BoundaryMethod m : b.getBoundaryMethods()) {
				if (m.getReturnType().equals(dtoBean)) {
					if (project.isAddBoundaryInterface())
						beans.add(b.getInterfaceName() + " -> Method: " + m.getName());
					else
						beans.add(b.getName() + " -> Method: " + m.getName());

					continue;
				}

				for (final MethodParameter p : m.getMethodParameters()) {
					if (p.getType().equals(dtoBean)) {
						if (project.isAddBoundaryInterface())
							beans.add(b.getInterfaceName() + " -> Method: " + m.getName());
						else
							beans.add(b.getName() + " -> Method: " + m.getName());

						break;
					}
				}
			}

		for (final DTOBean b : project.getAllDTOsOfProject())
			for (final DTOBeanAttribute a : b.getAttributes())
				if (a.getReferencedDTOBean() != null && a.getReferencedDTOBean().equals(dtoBean))
					dtos.add(b.getName() + " -> Attribute: " + a.getName());

		final var message = "The DTO cannot be removed, because it is referenced by following objects:\n";

		if (!forms.isEmpty()) {
			var formsMsg = "";

			for (final String s : forms)
				formsMsg += s + "\n";

			throw new IllegalStateException(message + formsMsg);
		}

		if (!beans.isEmpty()) {
			var beansMsg = "";

			for (final String s : beans)
				beansMsg += s + "\n";

			throw new IllegalStateException(message + beansMsg);
		}

		if (!dtos.isEmpty()) {
			var beansMsg = "";

			for (final String s : dtos)
				beansMsg += s + "\n";

			throw new IllegalStateException(message + beansMsg);
		}

		removeDTOBeanSourceFile(dtoBean);

		dtoBean.getNamespace().eResource().getContents().remove(dtoBean);

		EclipseIDEService.saveProjectMetaData(project);
	}

	/**
	 * Remove the DTO source file
	 * @param dtoBean
	 * @throws Exception if the DTO source file could not be deleted
	 */
	public void removeDTOBeanSourceFile(DTOBean dtoBean) throws Exception {
		EclipseIDEService.deleteSource(dtoBean.getSourceFile());

		if (dtoBean.getTypeScriptSourceFile() != null)
			EclipseIDEService.deleteWorkspaceFile(dtoBean.getTypeScriptSourceFile());
	}

	/**
	 * Search for a corresponding list DTO and create a new DTO if it doesn't exist yet
	 * @param domainObject
	 * @return the list DTO
	 */
	public DTOBean getOrCreateListDTO(DomainObject domainObject) {
		final String dtoName = domainObject.getName() + LIST_DTO_SUFFIX;

		// Check if this DTO already exists in the workspace
		for (final DTOBean b : project.getAllDTOsOfProject())
			if (b.getName().equals(dtoName) && b.getNamespace().getName().equals(domainObject.getNamespace().getName()))
				return b;

		// Initialize a new data transfer object
		final DTOBean listDTO = DtoFactory.eINSTANCE.createDTOBean();
		listDTO.setDomainObject(domainObject);
		listDTO.setPrimitive(false);
		listDTO.setMappable(false);
		listDTO.setStandardConversion(false);
		listDTO.setComment("Data transfer object for " + domainObject.getLabel() + " objects");
		listDTO.setName(dtoName);

		// Get the primary key attribute of this domain object
		final DomainAttribute pkAttribute = domainObject.getPKAttribute();

		final DTOBeanAttribute idAttribute = listDTO.addAttribute(pkAttribute, false);
		idAttribute.setSelectToken("a." + pkAttribute.getName());

		final DomainAttribute displayAttribute = listDTO.getDomainObject().getDisplayAttribute();

		if (displayAttribute != null) {
			final DTOBeanAttribute dtoDisplayAttribute = listDTO.addAttribute(displayAttribute, false);
			dtoDisplayAttribute.setSelectToken("a." + displayAttribute.getName());
		}

		return listDTO;
	}

	/**
	 * @return the role list DTO
	 */
	public DTOBean createRoleListDTO() {
		final DomainObject roleDomainObject = project.getDomainObjectByTag(DomainTagEnumeration.ROLE);

		if (roleDomainObject == null)
			throw new IllegalStateException("A domain object with the tag 'ROLE' could not be found!");

		final DTOBean roleDTO = getOrCreateListDTO(roleDomainObject);

		// Set the namespace of the DTO
		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			if (ns.getName().equals(roleDomainObject.getNamespace().getName())) {
				roleDTO.setNamespace(ns);
				break;
			}

		roleDTO.getNamespace().getJavaTypes().add(roleDTO);
		project.eResource().getContents().add(roleDTO);

		return roleDTO;
	}

	/**
	 * Search and delete unused virtual data transfer objects
	 * @throws Exception if an unexpected error has occurred
	 */
	public void removeUnusedVirtualDTOs() throws Exception {
		if (project.isBoundaryMode())
			return;

		// Search for all virtual data transfer objects
		final List<DTOBean> virtualDTOs = project.getAllDTOsOfProject().stream().filter(DTOBean::isVirtual).toList();

		for (final DTOBean dto : virtualDTOs) {
			// Check if a form uses this DTO
			boolean removeDTO = project.getAllFormsOfProject().stream().filter(form -> dto.equals(form.getDTO())).findFirst()
					.map(form -> false).orElse(true);

			// Check if this DTO is used by a facade method
			for (final BoundaryBean b : project.getAllBoundariesOfProject()) {
				for (final BoundaryMethod m : b.getBoundaryMethods()) {
					if (m.getReturnType().equals(dto)) {
						removeDTO = false;
						break;
					}

					for (final MethodParameter p : m.getMethodParameters())
						if (p.getType().equals(dto)) {
							removeDTO = false;
							break;
						}
				}

				if (!removeDTO)
					break;
			}

			// The DTO must not be deleted if it is referenced by another data transfer object!
			if (removeDTO)
				for (final DTOBean b : project.getAllDTOsOfProject()) {
					for (final DTOBeanAttribute a : b.getAttributes())
						if (a.getReferencedDTOBean() != null && a.getReferencedDTOBean().equals(dto)) {
							removeDTO = false;
							break;
						}

					if (!removeDTO)
						break;
				}

			if (removeDTO)
				removeDTO = !dto.isUsedByIntegrationMethod();

			// Remove the DTO from the meta-model if it is not referenced by other objects. But it is allowed to delete the source file
			// in any case!
			if (removeDTO)
				removeDTOBean(dto);
			else
				removeDTOBeanSourceFile(dto);
		}
	}

	/**
	 * Delete the selected DTO attribute
	 * @param dtoAttribute
	 * @throws IllegalStateException if the attribute must not be deleted
	 */
	public void deleteDTOAttribute(DTOBeanAttribute dtoAttribute) {
		final DTOBean dtoBean = dtoAttribute.getDTOBean();

		// The primary key DTO attribute must not be deleted!
		if (dtoBean.getPKAttribute().equals(dtoAttribute))
			throw new IllegalStateException("The primary key attribute must not be deleted!");

		// We must check if an existing DTO attribute is used somewhere else!
		final var forms = new ArrayList<String>();

		for (final Form form : project.getAllFormsOfProject()) {
			if (form.getFormType() == FormTypeEnumeration.TREE_VIEW) {
				final var treeView = (TreeView) form;

				// Search for references in all advanced search items
				forms.addAll(treeView.getAdvancedSearchItems().stream().filter(item -> item.getDTOAttribute().equals(dtoAttribute))
						.map(searchItem -> treeView.getName() + " -> Advanced search item: '" + searchItem.getLabel() + "'").toList());

				// Search for references in all quick-search items
				forms.addAll(treeView.getQuickSearchItems().stream().filter(item -> item.getDTOAttribute().equals(dtoAttribute))
						.map(searchItem -> treeView.getName() + " -> Quick search item: '" + searchItem.getLabel() + "'").toList());

				forms.addAll(searchDTOAttributeInTreeViewItem(treeView, dtoAttribute, treeView.getRootTreeItem()));

				continue;
			}

			form.getFormPanels().forEach(formPanel -> {
				FormTable formTable = null;

				if (formPanel.getBasePanel() != null)
					formTable = formPanel.getBasePanel().getFormTable();
				else
					formTable = formPanel.getFormTable();

				if (formTable != null) {
					// Test if the attribute is referenced by a table column field!
					for (final TableColumnField columnField : formTable.getFields())
						if (columnField.getDTOAttribute() != null && columnField.getDTOAttribute().equals(dtoAttribute))
							forms.add(form.getName() + " -> Field: " + columnField.getDTOAttribute().getName());
				}
				else {
					formPanel.getFields().forEach(formField -> {
						// Test if the attribute is referenced by a form field
						if (formField.getDTOAttribute().equals(dtoAttribute))
							forms.add(form.getName() + " -> Field: " + formField.getName());

						if (formField.getDTOAttribute().getReferencedDTOBean() != null) {
							// Test if the attribute belongs to a list DTO!
							final DTOBean refDTO = formField.getDTOAttribute().getReferencedDTOBean();

							// An attribute that belongs to a list DTO may not be deleted if it represents the primary key or the display
							// attribute!
							if (refDTO.equals(dtoBean)
									&& (dtoAttribute.equals(refDTO.getPKAttribute()) || dtoAttribute.equals(refDTO.getDisplayAttribute())))
								forms.add(form.getName() + " -> Field: " + formField.getName());
						}
					});
				}
			});
		}

		if (!forms.isEmpty()) {
			final var message = "The attribute cannot be removed, because it is referenced by following forms:\n";
			final String formMsg = forms.stream().reduce((r, t) -> r + "\n" + t).orElse("");

			throw new IllegalStateException(message + formMsg);
		}

		final Set<IntegrationTestCase> testCases = project.searchIntegrationTestCasesByMappingAttribute(dtoAttribute);

		if (!testCases.isEmpty()) {
			final var message = "The attribute cannot be removed, because it is referenced by following integration test cases:\n";
			final String testCaseNames = testCases.stream().map(IntegrationTestCase::getName)
					.collect(Collectors.joining(System.lineSeparator()));

			throw new IllegalStateException(message + testCaseNames);
		}

		dtoBean.getAttributes().remove(dtoAttribute);
	}

	/**
	 * Search for references of the given DTO attribute within a tree view item
	 * @param treeView
	 * @param attribute
	 * @param treeViewItem
	 * @return a list that contains all references of the attribute
	 */
	private List<String> searchDTOAttributeInTreeViewItem(TreeView treeView, DTOBeanAttribute attribute,
			TreeViewItem treeViewItem) {
		// Search for references in all display attributes
		final List<String> displayAttributes = treeViewItem.getDisplayAttributes().stream().filter(attr -> attr.equals(attribute))
				.map(attr -> treeView.getName() + " -> Display attribute: " + attribute.getName()).toList();

		final var itemList = new ArrayList<>(displayAttributes);

		// Search for references in all invisible attributes
		itemList.addAll(treeViewItem.getInvisibleAttributes().stream().filter(attr -> attr.equals(attribute))
				.map(attr -> treeView.getName() + " -> Invisible attribute: " + attribute.getName()).toList());

		// Search for references in all nodes
		itemList.addAll(treeViewItem.getNodes().stream().filter(node -> node.getDTOAttribute().equals(attribute))
				.map(node -> treeView.getName() + " -> Node: '" + node.getLabel() + "'").toList());

		// Search for references in all sub-items of this tree view item
		treeViewItem.getChildren()
				.forEach(subItem -> itemList.addAll(searchDTOAttributeInTreeViewItem(treeView, attribute, subItem)));

		return itemList;
	}

}
