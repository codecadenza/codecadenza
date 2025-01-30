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

import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.NUMBER;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.OBSERVABLE;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.STRING;
import static net.codecadenza.eclipse.shared.Constants.ANGULAR_COMMON_SERVICES_FOLDER;

import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for the authentication and authorization service of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularAuthServiceGenerator extends AbstractTypeScriptSourceGenerator {
	private final Project project;
	private final DTOBean logOnDTO;
	private final DomainObject logOnDomainObject;
	private final BoundaryBean logOnBoundary;

	/**
	 * Constructor
	 * @param project
	 */
	public AngularAuthServiceGenerator(Project project) {
		super("Service that provides authentication and authorization features");

		this.project = project;
		this.logOnDTO = project.getApplicationLogOnDTO();
		this.logOnDomainObject = logOnDTO.getDomainObject();
		this.logOnBoundary = project.getLogOnBoundary();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#getSourceFile()
	 */
	@Override
	public WorkspaceFile getSourceFile() {
		final var path = ANGULAR_COMMON_SERVICES_FOLDER + "/auth.service.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importTypes(Stream.of("BehaviorSubject", OBSERVABLE), "rxjs");
		importType("Injectable", "@angular/core");
		importType("RoleEnum", "../model/role.enum");
		importType("SHA256", "crypto-es/lib/sha256");
		importType(logOnDTO.getName(), "../../domain/" + logOnDTO.getName().toLowerCase() + ".interface");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("@Injectable({ providedIn: 'root'})");
		formatter.addLine("export class AuthService {");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final var logOnServicePath = "../../services/" + logOnDomainObject.getName().toLowerCase() + ".service";

		addPrivateConstant(NUMBER, "MIN_PASSWORD_LENGTH", "6").create();
		addPrivateConstant(STRING, "ITEM_NAME_USER", "'user'").create();
		addPrivateConstant(STRING, "ITEM_NAME_STATUS", "'loginstatus'").create();
		addPrivateConstant(STRING, "ITEM_NAME_ROLES", "'userroles'").create();

		if (project.isSpringBootApplication()) {
			addPrivateConstant(STRING, "ITEM_NAME_USER_NAME", "'username'").create();
			addPrivateConstant(STRING, "ITEM_NAME_USER_PASSWORD", "'password'").create();
		}
		else
			addPrivateConstant(STRING, "ITEM_NAME_CREDENTIALS", "'credentials'").create();

		addService(logOnDomainObject.getName() + "Service", logOnDomainObject.getLowerCaseName() + "Service", logOnServicePath);
		addService("I18NService", "i18n", "../../common/services/i18n.service");
		addService("Router", "router", "@angular/router");
		addService("MessageService", "messageService", "primeng/api");
		addService("NavigationHistoryService", "navigationHistoryService", "../../common/services/navigation-history.service");
		addField("BehaviorSubject<boolean>", "loginStatusSubject").create();

		addDependentDTO(logOnDTO);

		logOnDTO.getAttributes().forEach(attr -> {
			if (attr.getDomainAttribute() != null && attr.getDomainAttribute().getJavaType().isEnum())
				addDependentEnum((JavaEnum) attr.getDomainAttribute().getJavaType());

			if (attr.getReferencedDTOBean() != null)
				addDependentDTO(attr.getReferencedDTOBean());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addConstructorStatements(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addConstructorStatements(AngularContentFormatter formatter) {
		formatter.addLine("this.loginStatusSubject = new BehaviorSubject<boolean>(this.isLoggedIn());");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		addLoginMethod(formatter);
		addPermissionCheckMethod(formatter);
		addIsLoggedInMethod(formatter);
		addCheckClientMethod(formatter);
		addGetLoggedOnUserMethod(formatter);
		addLoginStatusChangedMethod(formatter);
		addChangePasswordMethod(formatter);
		addLogoutMethod(formatter);
	}

	/**
	 * Add the method for performing the login operation
	 * @param formatter
	 */
	private void addLoginMethod(AngularContentFormatter formatter) {
		final BoundaryMethod logOnMethod = logOnBoundary.getBoundaryMethodByReturnType(project.getApplicationLogOnDTO(),
				BoundaryMethodTypeEnumeration.LOG_ON);
		final var params = "userName, SHA256(password).toString()";

		var errorHandler = "error: () => this.messageService.add({ severity: 'error', ";
		errorHandler += "summary: this.i18n.translate('msg_errorlogin') })";

		formatter.addBlockComment("Perform the login operation");
		formatter.addLine("login(userName: string, password: string) {");
		formatter.increaseIndent();
		formatter.addLineComment("Save the credentials in the session storage before accessing the back-end for the first time");

		if (project.isSpringBootApplication()) {
			formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_USER_NAME, userName);");
			formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_USER_PASSWORD, SHA256(password).toString());");
		}
		else
			formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_CREDENTIALS, window.btoa(userName + ':' + password));");

		formatter.addBlankLine();
		formatter.addLine(new AngularServiceInvocationGenerator(logOnMethod).createInvocation(params) + ".subscribe({");
		formatter.increaseIndent();
		formatter.addLine("next: user => {");
		formatter.increaseIndent();
		formatter.addLine("const grantedRoles = [];");
		formatter.addBlankLine();
		formatter.addLineComment("tslint:disable-next-line:forin");
		formatter.addLine("for (const role in RoleEnum) {");
		formatter.increaseIndent();

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getAssociation() == null || attr.getAssociation().getTag() != AssociationTagEnumeration.USER_ROLE)
				continue;

			final String roleAssocName = attr.getName();
			final DTOBean roleDTO = attr.getReferencedDTOBean();

			for (final DTOBeanAttribute roleAttr : roleDTO.getAttributes()) {
				if (roleAttr.getDomainAttribute() == null || roleAttr.getDomainAttribute().getTag() != AttributeTagEnumeration.ROLE_NAME)
					continue;

				if (attr.getAssociation() instanceof ManyToManyAssociation) {
					formatter.addLine("for (const userRole of user." + roleAssocName + ") {");
					formatter.increaseIndent();
					formatter.addIfStatement("userRole." + roleAttr.getName() + " === role", "grantedRoles.push(RoleEnum[role]);", false);
					formatter.decreaseIndent();
					formatter.addLine("}");
				}
				else
					formatter.addIfStatement("user." + roleAssocName + "." + roleAttr.getName() + " === role",
							"grantedRoles.push(RoleEnum[role]);", false);

				break;
			}
		}

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
		formatter.addLineComment("Save the user data in the session storage");
		formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_USER, JSON.stringify(user));");
		formatter.addBlankLine();
		formatter.addLineComment("Save the status in the session storage");
		formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_STATUS, true.toString());");
		formatter.addBlankLine();
		formatter.addLineComment("Save the roles in the session storage");
		formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_ROLES, JSON.stringify(grantedRoles));");
		formatter.addBlankLine();
		formatter.addLine("this.loginStatusSubject.next(true);");
		formatter.addBlankLine();
		formatter.addLine("// Navigate to the start page if the login was successful");
		formatter.addLine("this.router.navigate(['/']);");
		formatter.decreaseIndent();
		formatter.addLine("},");
		formatter.addLine(errorHandler);
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method that is responsible for performing the permission check
	 * @param formatter
	 */
	private void addPermissionCheckMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Check if the user has at least one of the given roles granted");
		formatter.addLine("hasPermission(roles: RoleEnum[]): boolean {");
		formatter.increaseIndent();
		formatter.addLine("const rolesFromStorage = sessionStorage.getItem(AuthService.ITEM_NAME_ROLES);");
		formatter.addBlankLine();
		formatter.addIfStatement("!rolesFromStorage", "return false;", true);
		formatter.addLine("const grantedRoles = JSON.parse(rolesFromStorage);");
		formatter.addBlankLine();
		formatter.addIfStatement("!grantedRoles", "return false;", true);
		formatter.addLine("for (const role of roles) {");
		formatter.increaseIndent();
		formatter.addLine("for (const grantedRole of grantedRoles) {");
		formatter.increaseIndent();
		formatter.addIfStatement("grantedRole === role", "return true;", false);
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
		formatter.addLine("return false;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method that determines if a user is logged in
	 * @param formatter
	 */
	private void addIsLoggedInMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Return true if the user has logged in successfully");
		formatter.addLine("isLoggedIn(): boolean {");
		formatter.increaseIndent();
		formatter.addLineComment("Check if the session storage contains the login status");
		formatter.addLine("const statusFromSession = sessionStorage.getItem(AuthService.ITEM_NAME_STATUS);");
		formatter.addBlankLine();
		formatter.addIfStatement("!statusFromSession", "return false;", true);
		formatter.addLine("return statusFromSession === true.toString();");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method that returns the internal representation of the logged in user
	 * @param formatter
	 */
	private void addGetLoggedOnUserMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Return the user");
		formatter.addLine("getLoggedOnUser(): " + logOnDTO.getName() + " {");
		formatter.increaseIndent();
		formatter.addLine("const user = sessionStorage.getItem(AuthService.ITEM_NAME_USER);");
		formatter.addBlankLine();
		formatter.addIfStatement("!user", "throw new Error('User not found in session storage!');", true);
		formatter.addLine("return JSON.parse(user);");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method that that checks if the user belongs to a respective client
	 * @param formatter
	 */
	private void addCheckClientMethod(AngularContentFormatter formatter) {
		final DomainObject clientDomainObject = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT);
		DTOBeanAttribute clientAttr = null;

		if (clientDomainObject == null)
			return;

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getAssociation() == null || attr.getDomainAttribute() == null)
				continue;

			if (attr.getAssociation().getTarget().equals(clientDomainObject) && attr.getDomainAttribute().isPk()) {
				clientAttr = attr;
				break;
			}
		}

		if (clientAttr == null)
			return;

		final var clientAttrType = clientAttr.getDomainAttribute().getJavaType().isIntegerOrLong() ? "number" : "string";

		formatter.addBlockComment("Return true if the user belongs to the client with the given ID");
		formatter.addLine("checkClient(" + clientAttr.getName() + ": " + clientAttrType + "): boolean {");
		formatter.increaseIndent();
		formatter.addLine("return this.getLoggedOnUser()." + clientAttr.getName() + " === " + clientAttr.getName() + ";");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method to notify all listeners as soon as the login status has been changed
	 * @param formatter
	 */
	private void addLoginStatusChangedMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Notify all listeners as soon as the login status has been changed");
		formatter.addLine("onLoginStatusChanged(): " + OBSERVABLE + "<boolean> {");
		formatter.increaseIndent();
		formatter.addLine("return this.loginStatusSubject.asObservable();");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method to change the user's password
	 * @param formatter
	 */
	private void addChangePasswordMethod(AngularContentFormatter formatter) {
		final JavaType typeVoid = project.getJavaTypeByName(JavaType.VOID);
		final var pkTypeConv = logOnDomainObject.getPKAttribute().getJavaType().isIntegerOrLong() ? ".toString()" : "";
		final String pkAttrName = logOnDTO.getPKAttribute().getName();
		final var params = "user." + pkAttrName + pkTypeConv + ", oldPassword, newPassword, confirmedPassword";
		final var credentials = "user." + logOnDTO.getDisplayAttribute().getName() + " + ':' + newPassword";
		final BoundaryMethod changePasswordMethod = logOnBoundary.getBoundaryMethodByReturnType(typeVoid,
				BoundaryMethodTypeEnumeration.CHANGE_PASSWORD);
		final var errorMsgMinLength = "this.i18n.translate('msg_minpasswordlength', AuthService.MIN_PASSWORD_LENGTH.toString());";

		formatter.addBlockComment("Change the password");
		formatter.addLine("changePassword(oldPassword: string, newPassword: string, confirmedPassword: string) {");
		formatter.increaseIndent();
		formatter.addLine("const user = this.getLoggedOnUser();");
		formatter.addBlankLine();
		formatter.addLineComment("Check if the new and the confirmed passwords match");
		formatter.addLine("if (newPassword !== confirmedPassword) {");
		formatter.increaseIndent();
		formatter.addLine("const errorMsg = this.i18n.translate('msg_passwordmatch');");
		formatter.addLine("this.messageService.add({ severity: 'error', summary: errorMsg });");
		formatter.addLine("return;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
		formatter.addLineComment("Check the minimum password length");
		formatter.addLine("if (newPassword.length < AuthService.MIN_PASSWORD_LENGTH) {");
		formatter.increaseIndent();
		formatter.addLine("const errorMsg = " + errorMsgMinLength);
		formatter.addLine("this.messageService.add({ severity: 'error', summary: errorMsg });");
		formatter.addLine("return;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		if (project.isSpringBootApplication())
			formatter.addLineComment("Save the new password in the session storage");
		else
			formatter.addLineComment("Save the new credentials in the session storage");

		formatter.addLine(new AngularServiceInvocationGenerator(changePasswordMethod).createInvocation(params) + ".subscribe({");
		formatter.increaseIndent();
		formatter.addLine("error: error => this.messageService.add({ severity: 'error', summary: error.message }),");
		formatter.addLine("complete: () => {");
		formatter.increaseIndent();

		if (project.isSpringBootApplication())
			formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_USER_PASSWORD, SHA256(newPassword).toString());");
		else
			formatter.addLine("sessionStorage.setItem(AuthService.ITEM_NAME_CREDENTIALS, window.btoa(" + credentials + "));");

		formatter.addLine("this.navigationHistoryService.navigateToPreviousPage();");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the logout method
	 * @param formatter
	 */
	private void addLogoutMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Perform the logout operation");
		formatter.addLine("logout() {");
		formatter.increaseIndent();
		formatter.addLine("this.loginStatusSubject.next(false);");
		formatter.addBlankLine();
		formatter.addLineComment("Remove all items from the session storage!");
		formatter.addLine("sessionStorage.clear();");
		formatter.addBlankLine();
		formatter.addLine("this.router.navigate(['/login']);");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
