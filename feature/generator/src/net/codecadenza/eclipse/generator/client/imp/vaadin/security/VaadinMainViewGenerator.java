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
package net.codecadenza.eclipse.generator.client.imp.vaadin.security;

import static net.codecadenza.eclipse.generator.basic.client.imp.VaadinClientProjectFilesGenerator.MAIN_VIEW_COMMENT;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the main view of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinMainViewGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final VaadinI18NGenerator i18n;
	private DTOBeanAttribute userNameAttr;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinMainViewGenerator(Project project) {
		this.project = project;
		this.i18n = new VaadinI18NGenerator(project);

		for (final DTOBeanAttribute attr : project.getApplicationLogOnDTO().getAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_NAME) {
				this.userNameAttr = attr;
				break;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, MAIN_VIEW, project.getClientNamespace().toString());
		javaFile.setComment(MAIN_VIEW_COMMENT);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("com.vaadin.flow.component");
		importPackage("com.vaadin.flow.component.applayout");
		importPackage("com.vaadin.flow.component.avatar");
		importPackage("com.vaadin.flow.component.button");
		importPackage("com.vaadin.flow.component.html");
		importPackage("com.vaadin.flow.component.orderedlayout");
		importPackage("com.vaadin.flow.router");
		importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");

		addImports(new VaadinSecurityHelper(project).getSecurityImports());

		if (project.isSpringBootApplication())
			importPackage("org.springframework.security.core.context");
		else
			importPackage("com.vaadin.flow.server");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + MAIN_VIEW + " extends AppLayout ");

		if (project.isJakartaEEApplication())
			b.append("implements BeforeEnterObserver, AfterNavigationObserver");
		else
			b.append("implements AfterNavigationObserver");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();
		addPrivateField("I18NService", "i18n").inject().create();
		addPrivateField("TreeNavigator", "treeNavigator").create();
		addPrivateField("H1", "viewTitle").withDefaultValue("new H1()").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var logOutConfMsg = "Are you sure you want to log out? You will be redirected to the login page!";
		final String logOutTitleTrans = i18n.getI18NMessage("msg_title_logout", "Logout");
		final String logOutConfTrans = i18n.getI18NMessage("msg_conf_logout", logOutConfMsg);
		var b = new StringBuilder();

		if (project.isJakartaEEApplication()) {
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see com.vaadin.flow.router.BeforeEnterObserver#");
			b.append("beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("@Override\n");
			b.append("public void beforeEnter(BeforeEnterEvent event)\n");
			b.append("{\n");
			b.append("if(!securityManager.isLoggedIn())\n");
			b.append("event.forwardTo(LoginView.class);\n");
			b.append("}\n\n");

			addMethod("void beforeEnter(event)", b.toString());
		}

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("protected void onAttach(AttachEvent attachEvent)\n");
		b.append("{\n");
		b.append("treeNavigator = new TreeNavigator(i18n, " + MANAGED_SECURITY_MANAGER + ");\n\n");
		b.append("setPrimarySection(Section.DRAWER);\n");
		b.append("addToNavbar(true, createHeader());\n");
		b.append("addToDrawer(createSideMenu());\n\n");
		b.append("treeNavigator.buildTree();\n");
		b.append("}\n\n");

		addMethod("void onAttach(AttachEvent attachEvent)", b.toString());

		b = new StringBuilder();
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.AfterNavigationObserver#");
		b.append("afterNavigation(com.vaadin.flow.router.AfterNavigationEvent)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public void afterNavigation(AfterNavigationEvent event)\n");
		b.append("{\n");
		b.append("viewTitle.setText(getCurrentPageTitle());\n");
		b.append("}\n\n");

		addMethod("void afterNavigation()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the generated page header\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private Component createHeader()\n");
		b.append("{\n");
		b.append("final var avatar = new Avatar();\n");
		b.append("avatar.setName(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO()." + userNameAttr.getModelGetterName() + ");\n\n");
		b.append("final var cmdLogout = new Button(" + i18n.getI18NMessage("cmd_logout", "Logout") + ");\n");
		b.append("cmdLogout.setId(\"cmdLogout\");\n");
		b.append("cmdLogout.addClickListener(_ -> openLogoutDialog());\n\n");
		b.append("final var hlHeader = new HorizontalLayout();\n");
		b.append("hlHeader.setClassName(\"sidemenu-header\");\n");
		b.append("hlHeader.getThemeList().set(\"dark\", true);\n");
		b.append("hlHeader.setWidthFull();\n");
		b.append("hlHeader.setSpacing(false);\n");
		b.append("hlHeader.setAlignItems(FlexComponent.Alignment.CENTER);\n");
		b.append("hlHeader.add(new DrawerToggle(), viewTitle, avatar, cmdLogout);\n\n");
		b.append("return hlHeader;\n");
		b.append("}\n\n");

		addMethod("Component createHeader()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the generated side menu with a logo and the tree navigator\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private Component createSideMenu()\n");
		b.append("{\n");
		b.append("final var hlLogo = new HorizontalLayout();\n");
		b.append("hlLogo.setId(\"logo\");\n");
		b.append("hlLogo.setAlignItems(FlexComponent.Alignment.CENTER);\n");
		b.append("hlLogo.add(new Image(\"images/logo.png\", \"My App logo\"));\n");
		b.append("hlLogo.add(new H1(" + i18n.getI18NMessage("application_title", "My generated application") + "));\n\n");
		b.append("final var vlSideMenu = new VerticalLayout();\n");
		b.append("vlSideMenu.setClassName(\"sidemenu-menu\");\n");
		b.append("vlSideMenu.setSizeFull();\n");
		b.append("vlSideMenu.setPadding(false);\n");
		b.append("vlSideMenu.setSpacing(false);\n");
		b.append("vlSideMenu.setAlignItems(FlexComponent.Alignment.STRETCH);\n");
		b.append("vlSideMenu.add(hlLogo, treeNavigator);\n\n");
		b.append("return vlSideMenu;\n");
		b.append("}\n\n");

		addMethod("Component createSideMenu()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the title of the currently selected page\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private String getCurrentPageTitle()\n");
		b.append("{\n");
		b.append("if(getContent() instanceof final HasDynamicTitle content)\n");
		b.append("return content.getPageTitle();\n\n");
		b.append("final PageTitle title = getContent().getClass().getAnnotation(PageTitle.class);\n");
		b.append("return title == null ? \"\" : title.value();\n");
		b.append("}\n\n");

		addMethod("String getCurrentPageTitle()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Open logout window\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void openLogoutDialog()\n");
		b.append("{\n");
		b.append("final var dlg = new ConfirmationMessageDialog(" + logOutTitleTrans + ", " + logOutConfTrans);
		b.append(", " + i18n.getLocaleFragment() + ");\n");
		b.append("dlg.open();\n\n");
		b.append("dlg.setButtonClickListener(type ->\n");
		b.append("{\n");
		b.append("if(type != ButtonType.YES)\n");
		b.append("return;\n\n");

		addDebugLog(b, "Perform logout");

		b.append("\n");
		b.append(MANAGED_SECURITY_MANAGER + ".logOut();\n\n");
		b.append("try\n");
		b.append("{\n");

		if (project.isSpringBootApplication())
			b.append("SecurityContextHolder.getContext().setAuthentication(null);\n");
		else
			b.append("VaadinServletService.getCurrentServletRequest().getSession().invalidate();\n");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Logout failed!", "e");

		b.append("}\n\n");
		b.append("UI.getCurrent().getPage().setLocation(\"/" + project.getCode() + "\");\n");
		b.append("});\n");
		b.append("}\n\n");

		addMethod("void openLogoutDialog()", b.toString());

		i18n.save();
	}

}
