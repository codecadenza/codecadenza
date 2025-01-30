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
package net.codecadenza.eclipse.ui.dialog.util;

import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.util.Collection;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Proposal text field that displays all domain objects whose names match the entered filter text. By default, mapped superclasses
 * are not added to the proposal list!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectProposalTextField extends AbstractProposalTextField<DomainObject> {
	private final Project project;

	/**
	 * Constructor
	 * @param parent
	 * @param project
	 */
	public DomainObjectProposalTextField(Composite parent, Project project) {
		super(parent, SWT.BORDER, MIN_FILTER_LENGTH);

		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang.Object)
	 */
	@Override
	public String getProposalLabel(DomainObject domainObject) {
		return domainObject.getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalDescription(java.lang. Object)
	 */
	@Override
	public String getProposalDescription(DomainObject domainObject) {
		return "Package: " + domainObject.getNamespace().toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
	 */
	@Override
	public Collection<DomainObject> getProposalData(String filter) {
		return project.searchDomainObjectByName(filter, true, true, false);
	}

}
