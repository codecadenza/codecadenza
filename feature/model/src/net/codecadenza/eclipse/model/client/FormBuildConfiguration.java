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
package net.codecadenza.eclipse.model.client;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;

/**
 * <p>
 * Build configuration for creating forms and grid panels of a given domain object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormBuildConfiguration {
	private DomainObject domainObject;
	private FormTypeEnumeration createFormType;
	private FormTypeEnumeration viewFormType;
	private boolean createUpdateForm;
	private boolean createReadOnlyForm;
	private boolean createLOV;
	private boolean createGridPanels;
	private boolean shareDTOs;

	private final List<AbstractDomainAssociation> gridPanelAssociations = new ArrayList<>();

	/**
	 * @return the domain object
	 */
	public DomainObject getDomainObject() {
		return domainObject;
	}

	/**
	 * @param domainObject
	 */
	public void setDomainObject(DomainObject domainObject) {
		this.domainObject = domainObject;
	}

	/**
	 * @return the form type for creating new objects or null if this operation cannot be performed
	 */
	public FormTypeEnumeration getCreateFormType() {
		return createFormType;
	}

	/**
	 * @param createFormType
	 */
	public void setCreateFormType(FormTypeEnumeration createFormType) {
		this.createFormType = createFormType;
	}

	/**
	 * @return the form type for searching objects or null if this operation cannot be performed
	 */
	public FormTypeEnumeration getViewFormType() {
		return viewFormType;
	}

	/**
	 * @param viewFormType
	 */
	public void setViewFormType(FormTypeEnumeration viewFormType) {
		this.viewFormType = viewFormType;
	}

	/**
	 * @return true if an update form should be created
	 */
	public boolean isCreateUpdateForm() {
		return createUpdateForm;
	}

	/**
	 * @param createUpdateForm
	 */
	public void setCreateUpdateForm(boolean createUpdateForm) {
		this.createUpdateForm = createUpdateForm;
	}

	/**
	 * @return true if a read-only form should be created
	 */
	public boolean isCreateReadOnlyForm() {
		return createReadOnlyForm;
	}

	/**
	 * @param createReadOnlyForm
	 */
	public void setCreateReadOnlyForm(boolean createReadOnlyForm) {
		this.createReadOnlyForm = createReadOnlyForm;
	}

	/**
	 * @return true if a list of values should be created
	 */
	public boolean isCreateLOV() {
		return createLOV;
	}

	/**
	 * @param createLOV
	 */
	public void setCreateLOV(boolean createLOV) {
		this.createLOV = createLOV;
	}

	/**
	 * @return true if grid panels should be created
	 */
	public boolean isCreateGridPanels() {
		return createGridPanels;
	}

	/**
	 * @param createGridPanels
	 */
	public void setCreateGridPanels(boolean createGridPanels) {
		this.createGridPanels = createGridPanels;
	}

	/**
	 * @return a list containing all associations that are relevant for creating grid panels
	 */
	public List<AbstractDomainAssociation> getGridPanelAssociations() {
		return gridPanelAssociations;
	}

	/**
	 * @param assoc
	 */
	public void addGridPanelAssociation(AbstractDomainAssociation assoc) {
		gridPanelAssociations.add(assoc);

		createGridPanels = true;
	}

	/**
	 * @return true if data transfer object for forms of type CREATE/ADD, UPDATE and READONLY should be shared
	 */
	public boolean isShareDTOs() {
		return shareDTOs;
	}

	/**
	 * @param shareDTOs
	 */
	public void setShareDTOs(boolean shareDTOs) {
		this.shareDTOs = shareDTOs;
	}

	/**
	 * @return the total number of forms and grid panels that will be created based on the given configuration
	 */
	public int getNumberOfObjectsToBeProcessed() {
		int counter = 0;

		if (createUpdateForm)
			counter++;

		if (createLOV)
			counter++;

		if (createReadOnlyForm)
			counter++;

		if (createFormType != null)
			counter++;

		if (viewFormType != null)
			counter++;

		if (createGridPanels)
			counter += gridPanelAssociations.size();

		return counter;
	}

}
