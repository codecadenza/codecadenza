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

import java.util.Iterator;
import net.codecadenza.eclipse.diagram.domain.edit.commands.DomainInheritanceCreateCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.DomainObjectInheritanceReorientCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.EnumAssociationCreateCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.EnumAssociationReorientCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.ManyToManyAssociationCreateCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.ManyToManyAssociationReorientCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.ManyToOneAssociationCreateCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.ManyToOneAssociationReorientCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.OneToManyAssociationCreateCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.OneToManyAssociationReorientCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.OneToOneAssociationCreateCommand;
import net.codecadenza.eclipse.diagram.domain.edit.commands.OneToOneAssociationReorientCommand;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeCompartmentEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectInheritanceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gmf.runtime.emf.type.core.commands.DestroyElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateRelationshipRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.DestroyElementRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.ReorientRelationshipRequest;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.gmf.runtime.notation.View;

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
public class DomainObjectItemSemanticEditPolicy extends CodeCadenzaBaseItemSemanticEditPolicy {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy
	 * #getDestroyElementCommand(org.eclipse.gmf.runtime.emf.type.core.requests.DestroyElementRequest)
	 */
	@Override
	protected Command getDestroyElementCommand(DestroyElementRequest req) {
		final CompoundCommand cc = getDestroyEdgesCommand();

		addDestroyChildNodesCommand(cc);
		addDestroyShortcutsCommand(cc);

		final var view = (View) getHost().getModel();

		if (view.getEAnnotation("Shortcut") != null)
			req.setElementToDestroy(view);

		cc.add(getGEFWrapper(new DestroyElementCommand(req)));

		return cc.unwrap();
	}

	/**
	 * @param cmd
	 */
	protected void addDestroyChildNodesCommand(CompoundCommand cmd) {
		final var view = (View) getHost().getModel();
		final EAnnotation annotation = view.getEAnnotation("Shortcut");

		if (annotation != null)
			return;

		for (final Iterator<Node> it = view.getChildren().iterator(); it.hasNext();) {
			final Node node = it.next();

			switch (CodeCadenzaVisualIDRegistry.getVisualID(node)) {
				case DomainAttributeCompartmentEditPart.VISUAL_ID:
					for (final Iterator<Node> cit = node.getChildren().iterator(); cit.hasNext();) {
						final Node cnode = cit.next();

						if (CodeCadenzaVisualIDRegistry.getVisualID(cnode) == DomainAttributeEditPart.VISUAL_ID) {
							cmd.add(getDestroyElementCommand(cnode));
							break;
						}
					}

					break;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy#
	 * getCreateRelationshipCommand(org.eclipse.gmf.runtime.emf.type.core.requests.CreateRelationshipRequest)
	 */
	@Override
	protected Command getCreateRelationshipCommand(CreateRelationshipRequest req) {
		final Command command = req.getTarget() == null ? getStartCreateRelationshipCommand(req)
				: getCompleteCreateRelationshipCommand(req);

		return command != null ? command : super.getCreateRelationshipCommand(req);
	}

	/**
	 * @param req
	 * @return the command
	 */
	protected Command getStartCreateRelationshipCommand(CreateRelationshipRequest req) {
		if (CodeCadenzaElementTypes.OneToOneAssociation_4003 == req.getElementType())
			return getGEFWrapper(new OneToOneAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.ManyToManyAssociation_4002 == req.getElementType())
			return getGEFWrapper(new ManyToManyAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.ManyToOneAssociation_4004 == req.getElementType())
			return getGEFWrapper(new ManyToOneAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.OneToManyAssociation_4006 == req.getElementType())
			return getGEFWrapper(new OneToManyAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.EnumAssociation_4001 == req.getElementType())
			return getGEFWrapper(new EnumAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.DomainInheritance_4005 == req.getElementType())
			return getGEFWrapper(new DomainInheritanceCreateCommand(req, req.getSource(), req.getTarget()));

		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	protected Command getCompleteCreateRelationshipCommand(CreateRelationshipRequest req) {
		if (CodeCadenzaElementTypes.OneToOneAssociation_4003 == req.getElementType())
			return getGEFWrapper(new OneToOneAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.ManyToManyAssociation_4002 == req.getElementType())
			return getGEFWrapper(new ManyToManyAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.ManyToOneAssociation_4004 == req.getElementType())
			return getGEFWrapper(new ManyToOneAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.OneToManyAssociation_4006 == req.getElementType())
			return getGEFWrapper(new OneToManyAssociationCreateCommand(req, req.getSource(), req.getTarget()));

		if (CodeCadenzaElementTypes.EnumAssociation_4001 == req.getElementType())
			return null;

		if (CodeCadenzaElementTypes.DomainInheritance_4005 == req.getElementType())
			return getGEFWrapper(new DomainInheritanceCreateCommand(req, req.getSource(), req.getTarget()));

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy#
	 * getReorientRelationshipCommand(org.eclipse.gmf.runtime.emf.type.core.requests.ReorientRelationshipRequest)
	 */
	@Override
	protected Command getReorientRelationshipCommand(ReorientRelationshipRequest req) {
		switch (getVisualID(req)) {
			case OneToOneAssociationEditPart.VISUAL_ID:
				return getGEFWrapper(new OneToOneAssociationReorientCommand(req));
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return getGEFWrapper(new ManyToManyAssociationReorientCommand(req));
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return getGEFWrapper(new ManyToOneAssociationReorientCommand(req));
			case OneToManyAssociationEditPart.VISUAL_ID:
				return getGEFWrapper(new OneToManyAssociationReorientCommand(req));
			case EnumAssociationEditPart.VISUAL_ID:
				return getGEFWrapper(new EnumAssociationReorientCommand(req));
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return getGEFWrapper(new DomainObjectInheritanceReorientCommand(req));
		}

		return super.getReorientRelationshipCommand(req);
	}

}
