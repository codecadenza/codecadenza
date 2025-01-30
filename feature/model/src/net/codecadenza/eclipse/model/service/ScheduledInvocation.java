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
package net.codecadenza.eclipse.model.service;

/**
 * A representation of the model object '<em><b>Scheduled Invocation</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getSecond <em>Second</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getMinute <em>Minute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getHour <em>Hour</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfWeek <em>Day Of Week</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfMonth <em>Day Of Month</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getMonth <em>Month</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getYear <em>Year</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation()
 * @model
 * @generated
 */
public interface ScheduledInvocation extends MethodInvocation {
	/**
	 * Return the value of the '<em><b>Second</b></em>' attribute
	 * @return the value of the '<em>Second</em>' attribute
	 * @see #setSecond(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Second()
	 * @model
	 * @generated
	 */
	String getSecond();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getSecond <em>Second</em>}' attribute
	 * @param value the new value of the '<em>Second</em>' attribute
	 * @see #getSecond()
	 * @generated
	 */
	void setSecond(String value);

	/**
	 * Return the value of the '<em><b>Minute</b></em>' attribute
	 * @return the value of the '<em>Minute</em>' attribute
	 * @see #setMinute(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Minute()
	 * @model
	 * @generated
	 */
	String getMinute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getMinute <em>Minute</em>}' attribute
	 * @param value the new value of the '<em>Minute</em>' attribute
	 * @see #getMinute()
	 * @generated
	 */
	void setMinute(String value);

	/**
	 * Return the value of the '<em><b>Hour</b></em>' attribute
	 * @return the value of the '<em>Hour</em>' attribute
	 * @see #setHour(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Hour()
	 * @model
	 * @generated
	 */
	String getHour();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getHour <em>Hour</em>}' attribute
	 * @param value the new value of the '<em>Hour</em>' attribute
	 * @see #getHour()
	 * @generated
	 */
	void setHour(String value);

	/**
	 * Return the value of the '<em><b>Day Of Week</b></em>' attribute
	 * @return the value of the '<em>Day Of Week</em>' attribute
	 * @see #setDayOfWeek(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_DayOfWeek()
	 * @model
	 * @generated
	 */
	String getDayOfWeek();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfWeek <em>Day Of Week</em>}'
	 * attribute
	 * @param value the new value of the '<em>Day Of Week</em>' attribute
	 * @see #getDayOfWeek()
	 * @generated
	 */
	void setDayOfWeek(String value);

	/**
	 * Return the value of the '<em><b>Day Of Month</b></em>' attribute
	 * @return the value of the '<em>Day Of Month</em>' attribute
	 * @see #setDayOfMonth(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_DayOfMonth()
	 * @model
	 * @generated
	 */
	String getDayOfMonth();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfMonth <em>Day Of Month</em>}'
	 * attribute
	 * @param value the new value of the '<em>Day Of Month</em>' attribute
	 * @see #getDayOfMonth()
	 * @generated
	 */
	void setDayOfMonth(String value);

	/**
	 * Return the value of the '<em><b>Month</b></em>' attribute
	 * @return the value of the '<em>Month</em>' attribute
	 * @see #setMonth(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Month()
	 * @model
	 * @generated
	 */
	String getMonth();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getMonth <em>Month</em>}' attribute
	 * @param value the new value of the '<em>Month</em>' attribute
	 * @see #getMonth()
	 * @generated
	 */
	void setMonth(String value);

	/**
	 * Return the value of the '<em><b>Year</b></em>' attribute
	 * @return the value of the '<em>Year</em>' attribute
	 * @see #setYear(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Year()
	 * @model
	 * @generated
	 */
	String getYear();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getYear <em>Year</em>}' attribute
	 * @param value the new value of the '<em>Year</em>' attribute
	 * @see #getYear()
	 * @generated
	 */
	void setYear(String value);

}
