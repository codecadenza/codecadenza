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
package net.codecadenza.eclipse.generator.client.common.tree;

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Utility class that provides basic features for tree view generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeViewHelper {
	/**
	 * Prevent instantiation
	 */
	private TreeViewHelper() {

	}

	/**
	 * @param treeItem
	 * @return a list containing all boundary methods for downloading content from the server
	 */
	public static EList<BoundaryMethod> getDownloadMethods(TreeViewItem treeItem) {
		final DomainObject domainObject = treeItem.getItemDTO().getDomainObject();
		final Project project = domainObject.getNamespace().getProject();
		final var methods = new BasicEList<BoundaryMethod>();
		final BoundaryBean itemBoundary = project.getBoundaryByDomainObject(domainObject);

		if (itemBoundary == null)
			return methods;

		for (final DomainAttribute attr : treeItem.getItemDTO().getDomainObject().getAllLobAttributes()) {
			BoundaryMethod downloadMethod = null;

			// Search for a suitable boundary method to download the content
			for (final BoundaryMethod m : itemBoundary.getBoundaryMethods())
				if (m.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD && m.getDomainAttribute().equals(attr)) {
					if (addSecurity(project) && m.getPermissionMode() == PermissionModeEnumeration.DENY_ALL)
						break;

					downloadMethod = m;
					break;
				}

			if (downloadMethod != null)
				methods.add(downloadMethod);
		}

		return methods;
	}

	/**
	 * @param domainObject
	 * @param attr
	 * @return the download method or null if no appropriate method has been found
	 */
	public static BoundaryMethod getDownloadMethod(DomainObject domainObject, DomainAttribute attr) {
		final Project project = domainObject.getNamespace().getProject();
		final BoundaryBean itemBoundary = project.getBoundaryByDomainObject(domainObject);

		if (itemBoundary == null)
			return null;

		// Search for a suitable boundary method to download the content
		for (final BoundaryMethod m : itemBoundary.getBoundaryMethods())
			if (m.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD && m.getDomainAttribute().equals(attr)) {
				if (addSecurity(project) && m.getPermissionMode() == PermissionModeEnumeration.DENY_ALL)
					break;

				return m;
			}

		return null;
	}

	/**
	 * @param treeItem
	 * @return a form of type 'ADD' or null if no appropriate form has been found
	 */
	public static Form getAddForm(TreeViewItem treeItem) {
		final DomainObject domainObject = treeItem.getItemDTO().getDomainObject();
		final Project project = domainObject.getNamespace().getProject();

		for (final Form f : project.getAllFormsOfProject()) {
			if (f.getFormType() != FormTypeEnumeration.ADD)
				continue;

			if (!f.getDomainObject().equals(domainObject))
				continue;

			for (final FormField field : f.getAllFormFields()) {
				if (field.getDTOAttribute().getReferencedDTOBean() == null)
					continue;

				final DomainObject refDomainObject = field.getDTOAttribute().getReferencedDTOBean().getDomainObject();

				if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM) {
					if (!treeItem.isRootItem()) {
						if (refDomainObject.equals(treeItem.getAssociation().getDomainObject()))
							return f;
					}
					else if (refDomainObject.equals(domainObject))
						return f;
				}
			}
		}

		return null;
	}

	/**
	 * @param treeItem
	 * @return a form of type 'CREATE' or null if no appropriate form has been found
	 */
	public static Form getCreateNewForm(TreeViewItem treeItem) {
		final DomainObject domainObject = treeItem.getItemDTO().getDomainObject();
		final Project project = domainObject.getNamespace().getProject();

		for (final Form f : project.getAllFormsOfProject()) {
			if (f.getFormType() != FormTypeEnumeration.CREATE || !f.getDomainObject().equals(domainObject))
				continue;

			return f;
		}

		return null;
	}

	/**
	 * @param treeItem
	 * @return a form of type 'UPDATE' or null if no appropriate form has been found
	 */
	public static Form getEditForm(TreeViewItem treeItem) {
		return getEditForm(treeItem.getItemDTO().getDomainObject());
	}

	/**
	 * @param treeItem
	 * @return a form of type 'READONLY' or null if no appropriate form has been found
	 */
	public static Form getReadOnlyForm(TreeViewItem treeItem) {
		return getReadOnlyForm(treeItem.getItemDTO().getDomainObject());
	}

	/**
	 * @param domainObject
	 * @return a form of type 'UPDATE' or null if no appropriate form has been found
	 */
	public static Form getEditForm(DomainObject domainObject) {
		final Project project = domainObject.getNamespace().getProject();

		for (final Form f : project.getAllFormsOfProject()) {
			if (f.getFormType() != FormTypeEnumeration.UPDATE || !f.getDomainObject().equals(domainObject))
				continue;

			return f;
		}

		return null;
	}

	/**
	 * @param domainObject
	 * @return a form of type 'READONLY' or null if no appropriate form has been found
	 */
	public static Form getReadOnlyForm(DomainObject domainObject) {
		final Project project = domainObject.getNamespace().getProject();

		for (final Form f : project.getAllFormsOfProject()) {
			if (f.getFormType() != FormTypeEnumeration.READONLY || !f.getDomainObject().equals(domainObject))
				continue;

			return f;
		}

		return null;
	}

	/**
	 * Check if a delete method exists
	 * @param treeItem
	 * @return the delete method or null if no suitable method has been found
	 */
	public static BoundaryMethod getDeleteMethod(TreeViewItem treeItem) {
		final DTOBean dto = treeItem.getItemDTO();
		final Project project = treeItem.getItemDTO().getDomainObject().getNamespace().getProject();
		final BoundaryBean itemBoundary = project.getBoundaryByDomainObject(dto.getDomainObject());

		if (treeItem.getAssociation() instanceof ManyToManyAssociation)
			return null;

		if (itemBoundary == null)
			return null;

		for (final BoundaryMethod m : itemBoundary.getBoundaryMethods())
			if (m.getMethodType() == BoundaryMethodTypeEnumeration.DELETE) {
				if (addSecurity(project) && m.getPermissionMode() == PermissionModeEnumeration.DENY_ALL)
					return null;

				return m;
			}

		return null;
	}

	/**
	 * Check if tree item has a remove method
	 * @param treeItem
	 * @return the remove method or null if no suitable method has been found
	 */
	public static BoundaryMethod getSubItemRemoveMethod(TreeViewItem treeItem) {
		final Project project = treeItem.getItemDTO().getDomainObject().getNamespace().getProject();

		if (treeItem.isRootItem())
			return null;

		if (treeItem.getAssociation() instanceof ManyToManyAssociation
				|| treeItem.getAssociation() instanceof final OneToManyAssociation otm && !otm.isBidirectional()) {
			final BoundaryBean parentBoundary = project.getBoundaryByDomainObject(treeItem.getAssociation().getDomainObject());

			if (parentBoundary == null)
				return null;

			for (final BoundaryMethod m : parentBoundary.getBoundaryMethods())
				if (m.getMethodType() == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION
						&& m.getAssociation().equals(treeItem.getAssociation())) {
					if (addSecurity(project) && m.getPermissionMode() == PermissionModeEnumeration.DENY_ALL)
						return null;

					return m;
				}
		}

		return null;
	}

	/**
	 * @param project
	 * @return true if security for the given project is enabled
	 */
	private static boolean addSecurity(Project project) {
		return project.getApplicationLogOnDTO() != null && project.getLogOnBoundary() != null;
	}

}
