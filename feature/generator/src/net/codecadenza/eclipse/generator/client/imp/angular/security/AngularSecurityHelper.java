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
package net.codecadenza.eclipse.generator.client.imp.angular.security;

import java.util.List;
import java.util.Optional;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Security helper for Angular generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularSecurityHelper {
	private final boolean securityEnabled;

	/**
	 * Constructor
	 * @param project
	 */
	public AngularSecurityHelper(Project project) {
		this.securityEnabled = project.getApplicationLogOnDTO() != null;
	}

	/**
	 * @return true if security is enabled
	 */
	public boolean isSecurityEnabled() {
		return securityEnabled;
	}

	/**
	 * @param formatter
	 * @param roles
	 * @param source
	 */
	public void wrapSecurityCode(AngularContentFormatter formatter, List<Role> roles, String source) {
		if (!securityEnabled) {
			addIndent(formatter, source);
			return;
		}

		final Optional<String> roleList = roles.stream().map(r -> "RoleEnum." + r.getName()).reduce((r1, r2) -> r1 + ", " + r2);

		formatter.addLine("if (this.authService.hasPermission([" + roleList.orElse("") + "])) {");
		formatter.increaseIndent();

		addIndent(formatter, source);

		formatter.decreaseIndent();
		formatter.addLine("}");
	}

	/**
	 * @param form
	 * @return true if a client check method can be added to the given form
	 */
	public boolean addClientCheck(Form form) {
		if (!securityEnabled || !form.getDomainObject().isMandated())
			return false;

		return !(form.getFormType() == FormTypeEnumeration.ADD || form.getFormType() == FormTypeEnumeration.CREATE);
	}

	/**
	 * Add the method to check if the logged on user and the selected domain object belong to the same client
	 * @param formatter
	 * @param form
	 */
	public void addClientCheckMethod(AngularContentFormatter formatter, Form form) {
		final Project project = form.getDTO().getNamespace().getProject();
		final DomainObject clientDomainObject = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT);
		final var clientAccessor = new StringBuilder();

		if (!addClientCheck(form))
			return;

		formatter.addBlockComment("Check if the logged on user and the selected domain object belong to the same client!");
		formatter.addLine("override isObjectAllowed(object: " + form.getDTO().getName() + "): boolean {");
		formatter.increaseIndent();

		for (final DTOBeanAttribute dtoAttr : form.getDTO().getAttributes()) {
			final AbstractDomainAssociation assoc = dtoAttr.getAssociation();
			final DTOBean refDTO = dtoAttr.getReferencedDTOBean();

			if (assoc == null)
				continue;

			if (dtoAttr.getDomainAttribute() != null) {
				if (!assoc.getTarget().equals(clientDomainObject) || !dtoAttr.getDomainAttribute().isPk())
					continue;

				clientAccessor.append(dtoAttr.getName());
				break;
			}
			else if (refDTO != null && refDTO.getDomainObject().equals(clientDomainObject)) {
				clientAccessor.append(dtoAttr.getName() + "." + refDTO.getPKAttribute().getName());
				break;
			}
		}

		if (clientAccessor.toString().isEmpty()) {
			var comment = "ATTENTION: The generator was not able to add a client security check! ";
			comment += "Please consider to add it manually.";

			formatter.addLineComment(comment);
			formatter.addLine("return true;");
		}
		else
			formatter.addLine("return this.authService.checkClient(object." + clientAccessor + ");");

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the indent to all source code lines
	 * @param formatter
	 * @param source
	 */
	private void addIndent(AngularContentFormatter formatter, String source) {
		final String[] lines = source.split("\n");

		for (final String line : lines)
			if (!line.isEmpty())
				formatter.addLine(line);
			else
				formatter.addBlankLine();
	}

}
