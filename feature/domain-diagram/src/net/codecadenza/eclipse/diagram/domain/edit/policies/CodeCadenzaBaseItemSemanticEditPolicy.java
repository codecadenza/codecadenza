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

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import net.codecadenza.eclipse.diagram.domain.edit.helpers.CodeCadenzaBaseEditHelper;
import net.codecadenza.eclipse.diagram.domain.expressions.CodeCadenzaAbstractExpression;
import net.codecadenza.eclipse.diagram.domain.expressions.CodeCadenzaOCLFactory;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.java.JavaEnum;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.gef.requests.ReconnectRequest;
import org.eclipse.gmf.runtime.common.core.command.ICommand;
import org.eclipse.gmf.runtime.diagram.core.commands.DeleteCommand;
import org.eclipse.gmf.runtime.diagram.core.util.ViewUtil;
import org.eclipse.gmf.runtime.diagram.ui.commands.CommandProxy;
import org.eclipse.gmf.runtime.diagram.ui.commands.ICommandProxy;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.SemanticEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.requests.EditCommandRequestWrapper;
import org.eclipse.gmf.runtime.emf.commands.core.command.CompositeTransactionalCommand;
import org.eclipse.gmf.runtime.emf.type.core.ElementTypeRegistry;
import org.eclipse.gmf.runtime.emf.type.core.IEditHelperContext;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.gmf.runtime.emf.type.core.requests.ConfigureRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateElementRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateRelationshipRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.DestroyElementRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.DestroyReferenceRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.DestroyRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.DuplicateElementsRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.GetEditContextRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.IEditCommandRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.MoveRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.ReorientReferenceRelationshipRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.ReorientRelationshipRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.SetRequest;
import org.eclipse.gmf.runtime.notation.Edge;
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
public class CodeCadenzaBaseItemSemanticEditPolicy extends SemanticEditPolicy {
	public static final String VISUAL_ID_KEY = "visual_id";

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.SemanticEditPolicy#getCommand(org.eclipse.gef.Request)
	 */
	@Override
	public Command getCommand(Request request) {
		if (request instanceof final ReconnectRequest reconnectRequest) {
			final Object model = reconnectRequest.getConnectionEditPart().getModel();

			if (model instanceof final View view) {
				final int id = CodeCadenzaVisualIDRegistry.getVisualID(view);
				request.getExtendedData().put(VISUAL_ID_KEY, id);
			}
		}

		return super.getCommand(request);
	}

	/**
	 * @param request
	 * @return the visual ID from the request parameters
	 */
	protected int getVisualID(IEditCommandRequest request) {
		final Object id = request.getParameter(VISUAL_ID_KEY);

		return id instanceof Integer ? ((int) id) : -1;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.SemanticEditPolicy#getSemanticCommand(org.eclipse.gmf.runtime.emf.type.
	 * core.requests.IEditCommandRequest)
	 */
	@Override
	protected Command getSemanticCommand(IEditCommandRequest request) {
		final IEditCommandRequest completedRequest = completeRequest(request);
		Object editHelperContext = completedRequest.getEditHelperContext();

		if (editHelperContext instanceof View
				|| (editHelperContext instanceof final IEditHelperContext helperContext && helperContext.getEObject() instanceof View)) {
			// No semantic commands are provided for pure design elements
			return null;
		}

		if (editHelperContext == null)
			editHelperContext = ViewUtil.resolveSemanticElement((View) getHost().getModel());

		IElementType elementType = ElementTypeRegistry.getInstance().getElementType(editHelperContext);

		if (elementType == ElementTypeRegistry.getInstance().getType("org.eclipse.gmf.runtime.emf.type.core.default"))
			elementType = null;

		Command semanticCommand = getSemanticCommandSwitch(completedRequest);

		if (elementType != null) {
			if (semanticCommand != null) {
				final ICommand command = semanticCommand instanceof final ICommandProxy commandProxy ? commandProxy.getICommand()
						: new CommandProxy(semanticCommand);
				completedRequest.setParameter(CodeCadenzaBaseEditHelper.EDIT_POLICY_COMMAND, command);
			}

			ICommand command = elementType.getEditCommand(completedRequest);

			if (command != null) {
				if (!(command instanceof CompositeTransactionalCommand)) {
					final TransactionalEditingDomain editingDomain = ((IGraphicalEditPart) getHost()).getEditingDomain();
					command = new CompositeTransactionalCommand(editingDomain, command.getLabel()).compose(command);
				}

				semanticCommand = new ICommandProxy(command);
			}
		}

		boolean shouldProceed = true;

		if (completedRequest instanceof final DestroyRequest destroyRequest)
			shouldProceed = shouldProceed(destroyRequest);

		if (shouldProceed) {
			if (completedRequest instanceof DestroyRequest) {
				final TransactionalEditingDomain editingDomain = ((IGraphicalEditPart) getHost()).getEditingDomain();
				final Command deleteViewCommand = new ICommandProxy(new DeleteCommand(editingDomain, (View) getHost().getModel()));
				semanticCommand = semanticCommand == null ? deleteViewCommand : semanticCommand.chain(deleteViewCommand);
			}

			return semanticCommand;
		}

		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	protected Command getSemanticCommandSwitch(IEditCommandRequest req) {
		if (req instanceof final CreateRelationshipRequest createRelationshipRequest)
			return getCreateRelationshipCommand(createRelationshipRequest);
		else if (req instanceof final CreateElementRequest createElementRequest)
			return getCreateCommand(createElementRequest);
		else if (req instanceof final ConfigureRequest configureRequest)
			return getConfigureCommand(configureRequest);
		else if (req instanceof final DestroyElementRequest destroyElementRequest)
			return getDestroyElementCommand(destroyElementRequest);
		else if (req instanceof final DestroyReferenceRequest destroyReferenceRequest)
			return getDestroyReferenceCommand(destroyReferenceRequest);
		else if (req instanceof final DuplicateElementsRequest duplicateElementsRequest)
			return getDuplicateCommand(duplicateElementsRequest);
		else if (req instanceof final GetEditContextRequest getEditContextRequest)
			return getEditContextCommand(getEditContextRequest);
		else if (req instanceof final MoveRequest moveRequest)
			return getMoveCommand(moveRequest);
		else if (req instanceof final ReorientReferenceRelationshipRequest reorientReferenceRelationshipRequest)
			return getReorientReferenceRelationshipCommand(reorientReferenceRelationshipRequest);
		else if (req instanceof final ReorientRelationshipRequest reorientRelationshipRequest)
			return getReorientRelationshipCommand(reorientRelationshipRequest);
		else if (req instanceof final SetRequest setRequest)
			return getSetCommand(setRequest);

		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getConfigureCommand(ConfigureRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getCreateRelationshipCommand(CreateRelationshipRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getCreateCommand(CreateElementRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getSetCommand(SetRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getEditContextCommand(GetEditContextRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getDestroyElementCommand(DestroyElementRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getDestroyReferenceCommand(DestroyReferenceRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getDuplicateCommand(DuplicateElementsRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getMoveCommand(MoveRequest req) {
		return null;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getReorientReferenceRelationshipCommand(ReorientReferenceRelationshipRequest req) {
		return UnexecutableCommand.INSTANCE;
	}

	/**
	 * @param req
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected Command getReorientRelationshipCommand(ReorientRelationshipRequest req) {
		return UnexecutableCommand.INSTANCE;
	}

	/**
	 * @param cmd
	 * @return the command
	 */
	protected final Command getGEFWrapper(ICommand cmd) {
		return new ICommandProxy(cmd);
	}

	/**
	 * @param cmd
	 * @return the command
	 */
	protected final Command getMSLWrapper(ICommand cmd) {
		return getGEFWrapper(cmd);
	}

	/**
	 * @return the semantic element
	 */
	protected EObject getSemanticElement() {
		return ViewUtil.resolveSemanticElement((View) getHost().getModel());
	}

	/**
	 * @return the editing domain
	 */
	protected TransactionalEditingDomain getEditingDomain() {
		return ((IGraphicalEditPart) getHost()).getEditingDomain();
	}

	/**
	 * @param view
	 * @return the command
	 */
	protected Command getDestroyElementCommand(View view) {
		final EditPart editPart = getHost().getViewer().getEditPartRegistry().get(view);
		final var request = new DestroyElementRequest(getEditingDomain(), false);

		return editPart.getCommand(new EditCommandRequestWrapper(request, Collections.emptyMap()));
	}

	/**
	 * @return the compound command
	 */
	protected CompoundCommand getDestroyEdgesCommand() {
		final var cmd = new CompoundCommand();
		final var view = (View) getHost().getModel();

		for (final Iterator<Edge> it = view.getSourceEdges().iterator(); it.hasNext();)
			cmd.add(getDestroyElementCommand(it.next()));

		for (final Iterator<Edge> it = view.getTargetEdges().iterator(); it.hasNext();)
			cmd.add(getDestroyElementCommand(it.next()));

		return cmd;
	}

	/**
	 * @param command
	 */
	protected void addDestroyShortcutsCommand(CompoundCommand command) {
		final var view = (View) getHost().getModel();

		if (view.getEAnnotation("Shortcut") != null)
			return;

		for (final Iterator<View> it = view.getDiagram().getChildren().iterator(); it.hasNext();) {
			final View nextView = it.next();

			if (nextView.getEAnnotation("Shortcut") == null || !nextView.isSetElement() || nextView.getElement() != view.getElement())
				continue;

			command.add(getDestroyElementCommand(nextView));
		}
	}

	public static class LinkConstraints {
		private static final String OPPOSITE_END_VAR = "oppositeEnd";
		private static CodeCadenzaAbstractExpression DomainObjectInheritance_4005_TargetExpression;

		/**
		 * Prevent instantiation
		 */
		private LinkConstraints() {

		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a one-to-one association can be created
		 */
		public static boolean canCreateOneToOneAssociation_4003(DomainObject container, DomainObject source, DomainObject target) {
			return canExistOneToOneAssociation_4003(container, source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a many-to-many association can be created
		 */
		public static boolean canCreateManyToManyAssociation_4002(DomainObject container, DomainObject source, DomainObject target) {
			return canExistManyToManyAssociation_4002(container, source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a many-to-one association can be created
		 */
		public static boolean canCreateManyToOneAssociation_4004(DomainObject container, DomainObject source, DomainObject target) {
			return canExistManyToOneAssociation_4004(container, source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a one-to-many association can be created
		 */
		public static boolean canCreateOneToManyAssociation_4006(DomainObject container, DomainObject source, DomainObject target) {
			return canExistOneToManyAssociation_4006(container, source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if an enumeration association can be created
		 */
		public static boolean canCreateEnumAssociation_4001(DomainObject container, DomainObject source, JavaEnum target) {
			return canExistEnumAssociation_4001(container, source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a domain object inheritance association can be created
		 */
		public static boolean canCreateDomainObjectInheritance_4005(DomainObject container, DomainObject source,
				DomainObject target) {
			if (container != null && container.getInheritance() != null)
				return false;

			return canExistDomainObjectInheritance_4005(container, source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a one-to-one association can exist
		 */
		@SuppressWarnings("unused")
		public static boolean canExistOneToOneAssociation_4003(DomainObject container, DomainObject source, DomainObject target) {
			if (source.isMappedSuperClass())
				return false;

			return !(target != null && (target.isAbstract() || target.isMappedSuperClass()));
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a many-to-many association can exist
		 */
		@SuppressWarnings("unused")
		public static boolean canExistManyToManyAssociation_4002(DomainObject container, DomainObject source, DomainObject target) {
			return !isMappedSuperClass(source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a many-to-one association can exist
		 */
		@SuppressWarnings("unused")
		public static boolean canExistManyToOneAssociation_4004(DomainObject container, DomainObject source, DomainObject target) {
			return !isMappedSuperClass(source, target);
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a one-to-many association can exist
		 */
		@SuppressWarnings("unused")
		public static boolean canExistOneToManyAssociation_4006(DomainObject container, DomainObject source, DomainObject target) {
			return !isMappedSuperClass(source, target);
		}

		/**
		 * @param source
		 * @param target
		 * @return true if either the source or the target domain object represents a mapped superclass
		 */
		private static boolean isMappedSuperClass(DomainObject source, DomainObject target) {
			if (source.isMappedSuperClass())
				return true;

			return target != null && target.isMappedSuperClass();
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if an enumeration association can exist
		 */
		@SuppressWarnings("unused")
		public static boolean canExistEnumAssociation_4001(DomainObject container, DomainObject source, JavaEnum target) {
			return !source.isMappedSuperClass();
		}

		/**
		 * @param container
		 * @param source
		 * @param target
		 * @return true if a domain object inheritance association can exist
		 */
		@SuppressWarnings("unused")
		public static synchronized boolean canExistDomainObjectInheritance_4005(DomainObject container, DomainObject source,
				DomainObject target) {
			try {
				if (source.isMappedSuperClass())
					return false;

				if (target == null)
					return true;

				if (DomainObjectInheritance_4005_TargetExpression == null) {
					final Map<String, EClass> env = Collections.singletonMap(OPPOSITE_END_VAR, DomainPackage.eINSTANCE.getDomainObject());
					DomainObjectInheritance_4005_TargetExpression = CodeCadenzaOCLFactory.getExpression("self <> oppositeEnd\r\n",
							DomainPackage.eINSTANCE.getDomainObject(), env);
				}

				final Object targetVal = DomainObjectInheritance_4005_TargetExpression.evaluate(target,
						Collections.singletonMap(OPPOSITE_END_VAR, source));

				return targetVal instanceof final Boolean booleanValue && booleanValue.booleanValue();
			}
			catch (final Exception e) {
				CodeCadenzaDiagramEditorPlugin.getInstance().logError("Link constraint evaluation error", e);
				return false;
			}
		}
	}

}
