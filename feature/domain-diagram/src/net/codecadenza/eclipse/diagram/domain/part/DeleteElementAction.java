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
package net.codecadenza.eclipse.diagram.domain.part;

import java.util.List;
import java.util.Objects;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import net.codecadenza.eclipse.service.java.JavaEnumService;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.gmf.runtime.diagram.ui.actions.AbstractDeleteFromAction;
import org.eclipse.gmf.runtime.diagram.ui.actions.ActionIds;
import org.eclipse.gmf.runtime.diagram.ui.commands.CommandProxy;
import org.eclipse.gmf.runtime.diagram.ui.commands.ICommandProxy;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ConnectionNodeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.l10n.DiagramUIMessages;
import org.eclipse.gmf.runtime.emf.commands.core.command.CompositeTransactionalCommand;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Action to delete an element
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DeleteElementAction extends AbstractDeleteFromAction {
	private View view;
	private TransactionalEditingDomain domain;

	/**
	 * @param part
	 */
	public DeleteElementAction(IWorkbenchPart part) {
		super(part);
	}

	/**
	 * @param workbenchPage
	 */
	public DeleteElementAction(IWorkbenchPage workbenchPage) {
		super(workbenchPage);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.action.AbstractActionHandler#init()
	 */
	@Override
	public void init() {
		super.init();

		final ISharedImages workbenchImages = PlatformUI.getWorkbench().getSharedImages();

		setId(ActionIds.ACTION_DELETE_FROM_MODEL);
		setText(DiagramUIMessages.DiagramEditor_Delete_from_Model);
		setToolTipText(DiagramUIMessages.DiagramEditor_Delete_from_ModelToolTip);
		setHoverImageDescriptor(workbenchImages.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));
		setImageDescriptor(workbenchImages.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));
		setDisabledImageDescriptor(workbenchImages.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE_DISABLED));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.actions.DiagramAction#getCommandLabel()
	 */
	@Override
	protected String getCommandLabel() {
		return DiagramUIMessages.DiagramEditor_Delete_from_Model;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.actions.DiagramAction#getCommand(org.eclipse.gef.Request)
	 */
	@Override
	protected Command getCommand(Request request) {
		final List<EditPart> operationSet = getOperationSet();

		if (operationSet.isEmpty())
			return UnexecutableCommand.INSTANCE;

		final var command = new CompositeTransactionalCommand(getEditingDomain(), getCommandLabel());

		operationSet.stream().filter(Objects::nonNull).forEach(editPart -> {
			view = (View) editPart.getModel();

			if (editPart instanceof final ShapeNodeEditPart shapeNodeEditPart)
				domain = shapeNodeEditPart.getEditingDomain();
			else if (editPart instanceof final ConnectionNodeEditPart connectionNodeEditPart)
				domain = connectionNodeEditPart.getEditingDomain();

			final Command curCommand = editPart.getCommand(request);

			if (curCommand != null)
				command.compose(new CommandProxy(curCommand));
		});

		if (command.isEmpty() || command.size() != operationSet.size())
			return UnexecutableCommand.INSTANCE;

		return new ICommandProxy(command);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.actions.DiagramAction#doRun(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected void doRun(final IProgressMonitor progressMonitor) {
		if (domain == null)
			return;

		domain.getCommandStack().execute(new RecordingCommand(domain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				final EObject eObject = view.getElement();

				try {
					if (eObject instanceof final DomainObject bean)
						removeDomainObject(progressMonitor, bean);
					else if (eObject instanceof final DomainAttribute attribute)
						removeDomainAttribute(progressMonitor, attribute);
					else if (eObject instanceof final AbstractDomainAssociation association)
						removeDomainAssociation(progressMonitor, association);
					else if (eObject instanceof final JavaEnum javaEnum)
						removeJavaEnum(progressMonitor, javaEnum);
					else if (eObject instanceof final EnumLiteral literal)
						removeEnumLiteral(progressMonitor, literal);
					else if (eObject instanceof final EnumAssociation enumAssoc)
						removeEnumAssociation(progressMonitor, enumAssoc);
					else if (eObject instanceof DomainInheritance)
						removeInheritance();
				}
				catch (final Exception e) {
					CodeCadenzaDiagramEditorPlugin.getInstance().logError(e);

					throw new IllegalStateException(e);
				}
			}

			/**
			 * Remove the given domain object
			 * @param progressMonitor the progress monitor
			 * @param bean the domain object to remove
			 * @throws Exception if the domain object could not be removed
			 */
			private void removeDomainObject(final IProgressMonitor progressMonitor, DomainObject bean) throws Exception {
				final Project project = bean.getNamespace().getProject();

				new DomainObjectService(project).removeDomainObject(bean);

				doSuperRun(progressMonitor);

				if (project.isJavaSEApplication())
					ProjectBuildFactory.getBuildService(project).rebuildPersistenceUnit();
			}

			/**
			 * Remove the given domain attribute
			 * @param progressMonitor the progress monitor
			 * @param attribute the domain attribute to remove
			 * @throws Exception if the domain attribute could not be removed
			 */
			private void removeDomainAttribute(final IProgressMonitor progressMonitor, DomainAttribute attribute) throws Exception {
				final DomainObject bean = attribute.getDomainObject();
				final Project project = bean.getNamespace().getProject();
				final var domainObjectService = new DomainObjectService(project);

				// Avoid deleting an element if there are target inheritances
				if (bean.isMappedSuperClass() && bean.getTargetInheritances() != null && !bean.getTargetInheritances().isEmpty())
					throw new IllegalStateException(
							"The attribute cannot be deleted as long as other domain objects are derived from this class!");

				if (attribute.getJavaType() instanceof JavaEnum) {
					final var msg = new StringBuilder("This attribute has an enumeration type.\n");
					msg.append("It must be deleted by removing the respective connection!");

					throw new IllegalStateException(msg.toString());
				}

				domainObjectService.removeDomainAttribute(attribute);

				doSuperRun(progressMonitor);

				domainObjectService.rebuildDomainObjectSourceFiles(bean, true);
			}

			/**
			 * Remove the given domain association
			 * @param progressMonitor the progress monitor
			 * @param association the domain association to remove
			 * @throws Exception if the domain association could not be removed
			 */
			private void removeDomainAssociation(final IProgressMonitor progressMonitor, AbstractDomainAssociation association)
					throws Exception {
				final DomainObject bean = association.getDomainObject();
				final var domainObjectService = new DomainObjectService(bean.getNamespace().getProject());

				domainObjectService.removeDomainObjectAssociation(association);

				doSuperRun(progressMonitor);

				domainObjectService.rebuildDomainObjectSourceFiles(bean, false);
			}

			/**
			 * Remove the given Java enum
			 * @param progressMonitor the progress monitor
			 * @param javaEnum the Java enum to remove
			 * @throws Exception if the Java enum could not be removed
			 */
			private void removeJavaEnum(final IProgressMonitor progressMonitor, JavaEnum javaEnum) throws Exception {
				final Project project = javaEnum.getNamespace().getProject();

				new JavaEnumService(project).removeEnumeration(javaEnum);

				doSuperRun(progressMonitor);

				if (project.isJavaSEApplication())
					ProjectBuildFactory.getBuildService(project).rebuildPersistenceUnit();
			}

			/**
			 * Remove the given Java enum literal
			 * @param progressMonitor the progress monitor
			 * @param literal the Java enum literal to remove
			 * @throws Exception if the Java enum literal could not be removed
			 */
			private void removeEnumLiteral(final IProgressMonitor progressMonitor, EnumLiteral literal) throws Exception {
				final JavaEnum e = (JavaEnum) literal.eContainer();

				doSuperRun(progressMonitor);

				new JavaEnumService(e.getNamespace().getProject()).rebuildEnumerationSourceFile(e);
			}

			/**
			 * Remove the given Java enum association
			 * @param progressMonitor the progress monitor
			 * @param enumAssoc the Java enum association to remove
			 * @throws Exception if the Java enum association could not be removed
			 */
			private void removeEnumAssociation(final IProgressMonitor progressMonitor, EnumAssociation enumAssoc) throws Exception {
				final DomainAttribute attribute = enumAssoc.getDomainAttribute();

				if (attribute == null)
					throw new IllegalStateException(
							"The enum association could not be deleted as the corresponding domain attribute could not be found!");

				final var domainObjectService = new DomainObjectService(attribute.getDomainObject().getNamespace().getProject());
				domainObjectService.removeDomainAttribute(attribute);

				attribute.setColumn(null);

				doSuperRun(progressMonitor);
			}

			/**
			 * Remove the given domain object inheritance. Just inform the user that this functionality is not supported!
			 */
			private void removeInheritance() {
				final Shell shell = Display.getCurrent().getActiveShell();
				final var msgTitle = "Remove inheritance";

				MessageDialog.openInformation(shell, msgTitle, "This operation is not supported!");
			}
		});
	}

	/**
	 * @param progressMonitor
	 */
	private void doSuperRun(IProgressMonitor progressMonitor) {
		super.doRun(progressMonitor);
	}

}
