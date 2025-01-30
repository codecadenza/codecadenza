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
package net.codecadenza.eclipse.diagram.domain.edit.policies;

import net.codecadenza.eclipse.diagram.domain.edit.commands.DomainAttributeCreateCommand;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import org.eclipse.gef.commands.Command;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateElementRequest;

/**
 * <p>
 * Policy class
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAttributeCompartmentItemSemanticEditPolicy extends CodeCadenzaBaseItemSemanticEditPolicy {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy#getCreateCommand(org.eclipse.gmf.
	 * runtime.emf.type.core.requests.CreateElementRequest)
	 */
	@Override
	protected Command getCreateCommand(CreateElementRequest req) {
		if (CodeCadenzaElementTypes.DomainAttribute_3001 == req.getElementType()) {
			if (req.getContainmentFeature() == null)
				req.setContainmentFeature(DomainPackage.eINSTANCE.getDomainObject_Attributes());

			return getGEFWrapper(new DomainAttributeCreateCommand(req));
		}

		return super.getCreateCommand(req);
	}

}
