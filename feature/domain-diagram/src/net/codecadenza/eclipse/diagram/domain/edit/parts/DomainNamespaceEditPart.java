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
package net.codecadenza.eclipse.diagram.domain.edit.parts;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.edit.commands.CodeCadenzaCreateShortcutDecorationsCommand;
import net.codecadenza.eclipse.diagram.domain.edit.policies.DomainNamespaceCanonicalEditPolicy;
import net.codecadenza.eclipse.diagram.domain.edit.policies.DomainNamespaceItemSemanticEditPolicy;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.commands.Command;
import org.eclipse.gmf.runtime.diagram.ui.commands.ICommandProxy;
import org.eclipse.gmf.runtime.diagram.ui.editparts.DiagramEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.DiagramDragDropEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.EditPolicyRoles;
import org.eclipse.gmf.runtime.diagram.ui.requests.CreateViewRequest;
import org.eclipse.gmf.runtime.diagram.ui.requests.DropObjectsRequest;
import org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Edit part for domain namespaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainNamespaceEditPart extends DiagramEditPart {
	public static final String MODEL_ID = "CodeCadenza";
	public static final int VISUAL_ID = 1000;

	/**
	 * @param view
	 */
	public DomainNamespaceEditPart(View view) {
		super(view);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.DiagramEditPart#createDefaultEditPolicies()
	 */
	@Override
	protected void createDefaultEditPolicies() {
		super.createDefaultEditPolicies();

		installEditPolicy(EditPolicyRoles.SEMANTIC_ROLE, new DomainNamespaceItemSemanticEditPolicy());
		installEditPolicy(EditPolicyRoles.CANONICAL_ROLE, new DomainNamespaceCanonicalEditPolicy());

		installEditPolicy(EditPolicyRoles.DRAG_DROP_ROLE, new DiagramDragDropEditPolicy() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.DiagramDragDropEditPolicy#
			 * getDropObjectsCommand(org.eclipse.gmf.runtime.diagram.ui.requests.DropObjectsRequest)
			 */
			@Override
			public Command getDropObjectsCommand(DropObjectsRequest dropRequest) {
				final List<CreateViewRequest.ViewDescriptor> viewDescriptors = new ArrayList<>();

				for (final Object nextObject : dropRequest.getObjects()) {
					if (!(nextObject instanceof final EObject eObject))
						continue;

					viewDescriptors.add(
							new CreateViewRequest.ViewDescriptor(new EObjectAdapter(eObject), Node.class, null, getDiagramPreferencesHint()));
				}

				return createShortcutsCommand(dropRequest, viewDescriptors);
			}

			/**
			 * @param dropRequest
			 * @param viewDescriptors
			 * @return the command
			 */
			private Command createShortcutsCommand(DropObjectsRequest dropRequest,
					List<CreateViewRequest.ViewDescriptor> viewDescriptors) {
				final Command command = createViewsAndArrangeCommand(dropRequest, viewDescriptors);

				if (command != null)
					return command.chain(new ICommandProxy(
							new CodeCadenzaCreateShortcutDecorationsCommand(getEditingDomain(), (View) getModel(), viewDescriptors)));

				return null;
			}
		});
	}

}
