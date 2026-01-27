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
package net.codecadenza.eclipse.ui.dialog.boundary;

import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.FontDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Dialog for maintaining boundary beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditBoundaryBeanDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_BOUNDARY = "Edit boundary bean";
	private static final String DLG_TITLE_FACADE = "Edit facade bean";

	private final Project project;
	private final BoundaryBean boundaryBean;
	private final BoundaryService boundaryService;
	private final RepositoryService repositoryService;
	private Tree treeBoundary;
	private Tree treeRepository;
	private RepositoryMethod selectedRepositoryMethod;
	private String title = DLG_TITLE_FACADE;
	private Font virtualMethodFont;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param boundaryBean
	 */
	public EditBoundaryBeanDialog(Shell parentShell, BoundaryBean boundaryBean) {
		super(parentShell);

		this.boundaryBean = boundaryBean;
		this.project = boundaryBean.getNamespace().getProject();
		this.boundaryService = new BoundaryService(project);
		this.repositoryService = new RepositoryService(project);

		if (this.project.isBoundaryMode())
			this.title = DLG_TITLE_BOUNDARY;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var sashForm = new SashForm(panDialogArea, SWT.NONE);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panRepositoryMethods = new Composite(sashForm, SWT.NONE);
		panRepositoryMethods.setLayout(new GridLayout());

		final var lblRepository = new Label(panRepositoryMethods, SWT.NONE);
		lblRepository.setFont(JFaceResources.getBannerFont());
		lblRepository.setText("Available repository methods");

		final var gdRepository = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdRepository.widthHint = 400;
		gdRepository.heightHint = 500;

		treeRepository = new Tree(panRepositoryMethods, SWT.BORDER);
		treeRepository.setLayoutData(gdRepository);

		final var panBoundaryMethods = new Composite(sashForm, SWT.NONE);
		panBoundaryMethods.setLayout(new GridLayout());

		final var lblBoundary = new Label(panBoundaryMethods, SWT.NONE);
		lblBoundary.setFont(JFaceResources.getBannerFont());
		lblBoundary.setText("Existing bean methods");

		final var gdBoundary = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdBoundary.widthHint = 400;

		treeBoundary = new Tree(panBoundaryMethods, SWT.BORDER);
		treeBoundary.setLayoutData(gdBoundary);

		for (final RepositoryMethod repositoryMethod : boundaryBean.getRepository().getRepositoryMethods()) {
			// Do not add repositories that cannot be used by respective boundary bean methods
			final String repositoryMethodTypeName = repositoryMethod.getMethodType().getName();

			boolean addToTree = false;

			for (final BoundaryMethodTypeEnumeration boundaryType : BoundaryMethodTypeEnumeration.values())
				if (boundaryType.getName().equals(repositoryMethodTypeName)) {
					addToTree = true;
					break;
				}

			if (!project.isBoundaryMode() && repositoryMethod.getMethodType() == RepositoryMethodTypeEnumeration.GET_ASSOCIATION)
				addToTree = false;

			if (!addToTree)
				continue;

			final var methodItem = new TreeItem(treeRepository, SWT.NONE);
			methodItem.setData(repositoryMethod);
			methodItem.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PUBLIC));
			methodItem.setText(repositoryService.getMethodSignature(repositoryMethod));
		}

		addSaveMethodToTree();

		boundaryBean.getBoundaryMethods().forEach(boundaryMethod -> {
			final var methodItem = new TreeItem(treeBoundary, SWT.NONE);
			methodItem.setData(boundaryMethod);
			methodItem.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PUBLIC));
			methodItem.setText(boundaryService.getBoundaryMethodSignature(boundaryMethod));

			if (boundaryMethod.isVirtual()) {
				methodItem.setFont(virtualMethodFont);
				methodItem.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
			}
		});

		// Allow data to be copied or moved to the drop target
		int operations = DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT;
		final var target = new DropTarget(treeBoundary, operations);

		// Receive the data in text format
		final TextTransfer textTransfer = TextTransfer.getInstance();
		var types = new Transfer[] { textTransfer };

		target.setTransfer(types);

		target.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dropAccept(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dropAccept(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				try {
					addBoundaryMethod();
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		// Allow data to be copied or moved from the drag source
		operations = DND.DROP_MOVE | DND.DROP_COPY;
		final var source = new DragSource(treeRepository, operations);

		// Provide the data in text format
		types = new Transfer[] { TextTransfer.getInstance() };
		source.setTransfer(types);

		source.addDragListener(new DragSourceAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				if (getSelectedRepositoryMethod() == null)
					event.doit = false;
				else
					event.doit = true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				selectedRepositoryMethod = getSelectedRepositoryMethod();

				if (selectedRepositoryMethod == null)
					return;

				event.data = selectedRepositoryMethod.getName();
			}
		});

		virtualMethodFont = FontDescriptor.createFrom(sashForm.getFont()).setStyle(SWT.ITALIC)
				.createFont(this.getShell().getDisplay());

		sashForm.setWeights(1, 1);
		sashForm.addDisposeListener(_ -> virtualMethodFont.dispose());

		return panDialogArea;
	}

	/**
	 * @return the selected repository method
	 */
	private RepositoryMethod getSelectedRepositoryMethod() {
		final Optional<TreeItem> selItem = Stream.of(treeRepository.getSelection()).findFirst();

		if (!selItem.isPresent() || selItem.get().getData() == null
				|| !(selItem.get().getData() instanceof final RepositoryMethod repositoryMethod))
			return null;

		return repositoryMethod;
	}

	/**
	 * Add a new boundary method
	 * @throws Exception if the initialization of a new boundary method has failed
	 */
	protected void addBoundaryMethod() throws Exception {
		if (selectedRepositoryMethod == null)
			return;

		final Repository repository = boundaryBean.getRepository();
		final BoundaryMethod boundaryMethod = boundaryService.initializeBoundaryMethod(repository, selectedRepositoryMethod);

		final var dlg = new EditBoundaryMethodDialog(getShell(), boundaryBean, boundaryMethod);

		if (dlg.open() != Dialog.OK)
			return;

		boundaryMethod.setBoundaryBean(boundaryBean);
		boundaryBean.getBoundaryMethods().add(boundaryMethod);

		final var methodItem = new TreeItem(treeBoundary, SWT.NONE);
		methodItem.setData(boundaryMethod);
		methodItem.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PUBLIC));
		methodItem.setText(boundaryService.getBoundaryMethodSignature(boundaryMethod));

		if (boundaryMethod.isVirtual()) {
			methodItem.setFont(virtualMethodFont);
			methodItem.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID)
			try {
				EclipseIDEService.saveProjectMetaData(project);

				boundaryService.rebuildBoundarySourceFiles(boundaryBean);
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(title);
	}

	/**
	 * Add a temporary 'SAVE' repository method to the tree
	 */
	private void addSaveMethodToTree() {
		final DomainObject domainObject = boundaryBean.getDomainObject();

		// Save operations are not supported for abstract domain classes!
		if (domainObject.isAbstract())
			return;

		final RepositoryMethod methodSave = repositoryService.initializeSaveMethod(boundaryBean.getRepository());
		final String methodSignature = domainObject.getName() + " " + methodSave.getName() + "(" + domainObject.getName() + " "
				+ domainObject.getLowerCaseName() + ")";

		final var methodItem = new TreeItem(treeRepository, SWT.NONE);
		methodItem.setData(methodSave);
		methodItem.setImage(JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PUBLIC));
		methodItem.setText(methodSignature);
	}

}
